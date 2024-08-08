{-# LANGUAGE PatternSynonyms #-}

module LR0 (LR0Item (..), new_item, new_item_with_index_0, generate) where

import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar
import Item (Item)
import qualified Item
import StateTable (Action (..), StateTable)
import qualified StateTable.Generation
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
                            (more ++ filter (\i -> not (Set.member i kernel) && not (Set.member i current_closure)) (Set.toList to_add_to_closure))
                    _ -> go current_closure more

generate :: Grammar -> StateTable LR0Item ()
generate grammar =
    StateTable.Generation.generate
        new_item_with_index_0
        ( ( \(LR0Item rule@(Rule _ r_nt _) _) ->
                let action = if r_nt == Grammar.augment_nt grammar then Accept else Reduce rule
                in Grammar.all_terminals grammar & Set.toList & map (\term -> (term, action)) & Map.fromList
          )
        )
        grammar
