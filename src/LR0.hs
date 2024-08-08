{-# LANGUAGE PatternSynonyms #-}

module LR0 (LR0Item (..), new_item, new_item_with_index_0, generate) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State as StateMonad
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar
import Item (Item)
import qualified Item
import ItemSet (ItemSet)
import qualified ItemSet
import StateTable (Action (..), ActionOrConflict (..), State (..), StateTable)
import qualified StateTable
import Symbols (Symbol (..))

data LR0Item = LR0Item Rule Int deriving (Show, Eq, Ord)
new_item :: Rule -> Int -> Maybe LR0Item
new_item r@(Rule _ _ production) i
    | i > length production = Nothing
    | otherwise = Just $ LR0Item r i
new_item_with_index_0 :: Rule -> LR0Item
new_item_with_index_0 r = LR0Item r 0

instance Item LR0Item where
    rule (LR0Item r _) = r
    index (LR0Item _ i) = i

    sym_after_dot (LR0Item (Rule _ _ production) index)
        | index < length production = Just (production !! index)
        | otherwise = Nothing

    can_move_forward (LR0Item (Rule _ _ r_production) i) = i <= length (r_production)
    move_forward (LR0Item r i) = new_item r (i + 1)

    sets_equal = (==)
    find_closure grammar kernel = go Set.empty (Set.toList kernel)
        where
            go current_closure [] = current_closure
            go current_closure (current_item : more) =
                let symbol_after_dot = Item.sym_after_dot current_item
                in case symbol_after_dot of
                    Just (S'NonTerminal nt_after_dot) ->
                        let to_add_to_closure = Set.map new_item_with_index_0 $ Set.fromList $ Grammar.filter_rules_with_nt nt_after_dot grammar
                        in go
                            (current_closure <> to_add_to_closure)
                            (more ++ filter (\i -> not (Set.member i kernel) && not (Set.member i current_closure)) (Set.toAscList to_add_to_closure))
                    _ -> go current_closure more

generate :: Grammar -> StateTable LR0Item ()
generate grammar =
    StateMonad.evalState
        ( do
            (first_set, _) <-
                StateMonad.state $ ItemSet.new grammar (Set.map (\rule -> new_item_with_index_0 rule) (Set.fromList $ Grammar.augment_rules grammar))
            go [] [first_set]
        )
        ItemSet.new_interner
    where
        go :: [State LR0Item ()] -> [ItemSet LR0Item] -> StateMonad.State (ItemSet.Interner LR0Item) (StateTable LR0Item ())
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
                                    (next_set, set_is_new) <- StateMonad.state $ ItemSet.new grammar new_kernel

                                    pure ([Map.singleton term (Shift $ ItemSet.number next_set)], if set_is_new then [next_set] else [])
                                Nothing ->
                                    pure
                                        ( map
                                            ( \(LR0Item rule@(Rule _ r_nt _) _) ->
                                                let action = if r_nt == Grammar.augment_nt grammar then Accept else Reduce rule
                                                in Grammar.all_terminals grammar & Set.toList & map (\term -> (term, action)) & Map.fromList
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
                                    (next_set, set_is_new) <- StateMonad.state $ ItemSet.new grammar new_kernel

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
