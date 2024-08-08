{-# LANGUAGE PatternSynonyms #-}

module LR1 (LR1Item (..), new_item, new_item_with_index_0, generate) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State as StateMonad
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import FirstAndFollowSets (find_firsts, find_follows)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar
import Item (Item)
import qualified Item
import ItemSet (ItemSet)
import qualified ItemSet
import qualified LR0
import StateTable (Action (..), ActionOrConflict (..), State (..), StateTable)
import qualified StateTable
import Symbols (Symbol (..), Terminal (..))

data LR1Item = LR1Item Rule Int Terminal deriving (Show, Eq, Ord)
new_item :: Rule -> Int -> Terminal -> Maybe LR1Item
new_item r@(Rule _ _ production) i l
    | i > length production = Nothing
    | otherwise = Just $ LR1Item r i l
new_item_with_index_0 :: Rule -> Terminal -> LR1Item
new_item_with_index_0 r l = LR1Item r 0 l

instance Item LR1Item where
    rule (LR1Item r _ _) = r
    index (LR1Item _ i _) = i

    sym_after_dot (LR1Item (Rule _ _ production) index _)
        | index < length production = Just (production !! index)
        | otherwise = Nothing

    can_move_forward (LR1Item (Rule _ _ r_production) i _) = i <= length (r_production)
    move_forward (LR1Item r i l) = new_item r (i + 1) l

    sets_equal = (==)
    find_closure grammar kernel =
        lr0_closure
            & Set.map (\(LR0.LR0Item rule@(Rule _ nt _) index) -> follow_sets Map.! nt & Set.map (\lookahead -> LR1Item rule index lookahead))
            & Set.unions
        where
            lr0_closure =
                kernel
                    & Set.map (\(LR1Item ru i _) -> fromJust $ LR0.new_item ru i) -- fromJust is safe because the item already exists as an SLR item so the index must be valid
                    & Item.find_closure grammar
            first_sets = find_firsts grammar
            follow_sets =
                find_follows
                    ( kernel
                        & Set.map (\(LR1Item (Rule _ nt _) _ lookahead) -> Map.singleton nt (Set.singleton lookahead))
                        & Map.unionsWith (<>)
                    )
                    ((kernel & Set.map Item.rule & Set.toList) <> (lr0_closure & Set.map Item.rule & Set.toList))
                    first_sets

generate :: Grammar -> StateTable LR1Item ()
generate grammar =
    StateMonad.evalState
        ( do
            (first_set, _) <-
                StateMonad.state $ ItemSet.new grammar (Set.map (\rule -> new_item_with_index_0 rule EOF) (Set.fromList $ Grammar.augment_rules grammar))
            go [] [first_set]
        )
        ItemSet.new_interner
    where
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
                                    (next_set, set_is_new) <- StateMonad.state $ ItemSet.new grammar new_kernel

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
