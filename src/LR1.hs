{-# LANGUAGE PatternSynonyms #-}

module LR1 (LR1Item (..), new_item, new_item_with_index_0, generate) where

import Data.Function ((&))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import FirstAndFollowSets (TerminalOrEpsilon (..), find_firsts, firsts_of_sequence)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar
import Item (Item)
import qualified Item
import StateTable (Action (..), StateTable)
import qualified StateTable.Generation
import Symbols (Symbol (..), Terminal (..))

data LR1Item = LR1Item Rule Int (Set Terminal) deriving (Show, Eq, Ord)
new_item :: Rule -> Int -> Set Terminal -> Maybe LR1Item
new_item r@(Rule _ _ production) i l
    | i > length production = Nothing
    | otherwise = Just $ LR1Item r i l
new_item_with_index_0 :: Rule -> Set Terminal -> LR1Item
new_item_with_index_0 r l = LR1Item r 0 l

instance Item LR1Item where
    rule (LR1Item r _ _) = r
    index (LR1Item _ i _) = i

    sym_after_dot (LR1Item (Rule _ _ production) index _)
        | index < length production = Just (production !! index)
        | otherwise = Nothing

    can_move_forward (LR1Item (Rule _ _ r_production) i _) = i <= length (r_production)
    move_forward (LR1Item r i l) = new_item r (i + 1) l

    find_closure grammar kernel = go Set.empty (Set.toList kernel)
        where
            go current_closure [] = current_closure
            go current_closure (current_item@(LR1Item (Rule _ _ prod) index current_item_lookahead) : more) =
                let symbol_after_dot = Item.sym_after_dot current_item
                    symbols_after = drop (index + 1) prod
                    lookaheads =
                        symbols_after
                            & firsts_of_sequence first_sets
                            & Set.map
                                ( \case
                                    ToE'Terminal t -> Set.singleton t
                                    ToE'Epsilon -> current_item_lookahead
                                )
                            & Set.unions
                in case symbol_after_dot of
                    Just (S'NonTerminal nt_after_dot) ->
                        let to_add_to_closure = Set.map (\r -> new_item_with_index_0 r lookaheads) (Set.fromList $ Grammar.filter_rules_with_nt nt_after_dot grammar)
                        in go
                            (current_closure <> to_add_to_closure)
                            (more ++ filter (\i -> not (Set.member i kernel) && not (Set.member i current_closure)) (Set.toList to_add_to_closure))
                    _ -> go current_closure more

            first_sets = find_firsts grammar

generate :: Grammar -> StateTable LR1Item ()
generate grammar =
    StateTable.Generation.generate
        (\rule -> new_item_with_index_0 rule (Set.singleton EOF))
        StateTable.Generation.default_intern
        ( \(LR1Item rule@(Rule _ r_nt _) _ lookaheads) ->
            if r_nt == Grammar.augment_nt grammar
                then lookaheads & Set.toList & map (\l -> (l, Accept)) & Map.fromList
                else lookaheads & Set.toList & map (\l -> (l, Reduce rule)) & Map.fromList
        )
        grammar
