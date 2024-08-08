{-# LANGUAGE PatternSynonyms #-}

module SLR (generate) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State as StateMonad
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import FirstAndFollowSets (find_firsts, find_follows)
import Grammar (Grammar, pattern Rule)
import qualified Grammar
import Item (LR1Item, pattern LR1Item)
import qualified Item
import ItemSet (ItemSet, augment_items, new_item_set_lr1_with_follows)
import qualified ItemSet
import StateTable (Action (..), ActionOrConflict (..), State (..), StateTable)
import qualified StateTable
import Symbols (Symbol (..), Terminal (..))

generate :: Grammar -> StateTable LR1Item ()
generate grammar =
    StateMonad.evalState
        ( do
            (first_set, _) <- StateMonad.state $ new_item_set_lr1_with_follows follow_sets (Grammar.all_rules grammar) (Set.map (\i -> Item.lr0_to_lr1 i EOF) $ augment_items grammar)
            go [] [first_set]
        )
        ItemSet.new_interner
    where
        first_sets = find_firsts grammar
        follow_sets = find_follows (Map.singleton (Grammar.augment_nt grammar) (Set.singleton EOF)) (Grammar.all_rules grammar) first_sets

        go :: [State LR1Item ()] -> [ItemSet LR1Item] -> StateMonad.State (ItemSet.Interner LR1Item) (StateTable LR1Item ())
        go states (current_set : more_sets) = do
            let current_set_number = ItemSet.number current_set
            let symbols_after_dot =
                    ItemSet.item_set_items current_set
                        & Set.map (\item -> Map.singleton (Item.sym_after_dot item) (Set.singleton item))
                        & Map.unionsWith (<>)

            (actions, new_sets_from_actions) <-
                symbols_after_dot
                    & Map.toList
                    & mapM
                        ( \(symbol_after_dot, items) ->
                            case symbol_after_dot of
                                Just (S'Terminal term) -> do
                                    -- fromJust should be safe because the symbol after the dot is a terminal
                                    let new_kernel = Set.map (fromJust . Item.move_forward) items
                                    (next_set, set_is_new) <- StateMonad.state $ new_item_set_lr1_with_follows follow_sets (Grammar.all_rules grammar) new_kernel

                                    pure ([Map.singleton term (Shift $ ItemSet.number next_set)], if set_is_new then [next_set] else [])
                                Nothing ->
                                    pure
                                        ( map
                                            ( \(LR1Item rule@(Rule _ r_nt _) _ lookahead) ->
                                                if r_nt == Grammar.augment_nt grammar
                                                    then Map.singleton lookahead Accept
                                                    else Map.singleton lookahead (Reduce rule)
                                            )
                                            (Set.toList items)
                                        , []
                                        )
                                _ -> pure ([], [])
                        )
                    & fmap unzip
                    & fmap (first (Map.unionsWith (<>) . map (Map.map SingleAction) . concat))
                    & fmap (second concat)

            (gotos, new_sets_from_gotos) <-
                symbols_after_dot
                    & Map.toList
                    & mapM
                        ( \(symbol_after_dot, items) ->
                            case symbol_after_dot of
                                Just (S'NonTerminal nt) -> do
                                    let new_kernel = Set.map (fromJust . Item.move_forward) items
                                    (next_set, set_is_new) <- StateMonad.state $ new_item_set_lr1_with_follows follow_sets (Grammar.all_rules grammar) new_kernel

                                    pure (Map.singleton nt (ItemSet.number next_set), if set_is_new then [next_set] else [])
                                _ -> pure (Map.empty, [])
                        )
                    & fmap unzip
                    & fmap (first (Map.unionsWith (<>) . map (Map.map SingleAction)))
                    & fmap (second concat)

            go
                (State current_set_number current_set actions gotos : states)
                (more_sets ++ new_sets_from_actions ++ new_sets_from_gotos)
        go states [] =
            case StateTable.new states of
                Right st -> pure st
                Left (StateTable.DuplicateState s1 s2) -> error $ "duplicate state: " ++ show s1 ++ " " ++ show s2

