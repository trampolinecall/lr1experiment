{-# LANGUAGE PatternSynonyms #-}

module LR1 (LR1Item (..), new_item, new_item_with_index_0, generate) where

import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

import FirstAndFollowSets (find_firsts, find_follows)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar
import Item (Item)
import qualified Item
import qualified LR0
import StateTable (Action (..), StateTable)
import qualified StateTable.Generation
import Symbols (Terminal (..))

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

    find_closure grammar kernel =
        lr0_closure
            & Set.map (\(LR0.LR0Item rule@(Rule _ nt _) index) -> LR1Item rule index (follow_sets Map.! nt))
        where
            lr0_closure =
                kernel
                    & Set.map (\(LR1Item ru i _) -> fromJust $ LR0.new_item ru i) -- fromJust is safe because the item already exists as an LR1 item so the index must be valid
                    & Item.find_closure grammar
            first_sets = find_firsts grammar
            follow_sets =
                find_follows
                    ( kernel
                        & Set.map (\(LR1Item (Rule _ nt _) _ lookaheads) -> Map.singleton nt lookaheads)
                        & Map.unionsWith (<>)
                    )
                    ((kernel & Set.map Item.rule & Set.toList) <> (lr0_closure & Set.map Item.rule & Set.toList))
                    first_sets

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
