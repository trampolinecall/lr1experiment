{-# LANGUAGE PatternSynonyms #-}

module SLR (SLRItem (..), new_item, new_item_with_index_0, generate) where

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

data SLRItem = SLRItem Rule Int (Set Terminal) deriving (Show, Eq, Ord)
new_item :: Rule -> Int -> Set Terminal -> Maybe SLRItem
new_item r@(Rule _ _ production) i l
    | i > length production = Nothing
    | otherwise = Just $ SLRItem r i l
new_item_with_index_0 :: Rule -> Set Terminal -> SLRItem
new_item_with_index_0 r l = SLRItem r 0 l

instance Item SLRItem where
    rule (SLRItem r _ _) = r
    index (SLRItem _ i _) = i

    sym_after_dot (SLRItem (Rule _ _ production) index _)
        | index < length production = Just (production !! index)
        | otherwise = Nothing

    can_move_forward (SLRItem (Rule _ _ r_production) i _) = i <= length (r_production)
    move_forward (SLRItem r i l) = new_item r (i + 1) l

    find_closure grammar kernel =
        lr0_closure
            & Set.map (\(LR0.LR0Item rule@(Rule _ nt _) index) -> SLRItem rule index (grammar_follow_sets Map.! nt))
        where
            lr0_closure =
                kernel
                    & Set.map (\(SLRItem ru i _) -> fromJust $ LR0.new_item ru i) -- fromJust is safe because the item already exists as an SLR item so the index must be valid
                    & Item.find_closure grammar
            grammar_follow_sets =
                find_follows
                    (Map.singleton (Grammar.augment_nt grammar) (Set.singleton EOF))
                    (Grammar.all_rules grammar)
                    (find_firsts grammar)

generate :: Grammar -> StateTable SLRItem ()
generate grammar =
    StateTable.Generation.generate
        (\rule -> new_item_with_index_0 rule (Set.singleton EOF))
        StateTable.Generation.default_intern
        ( \(SLRItem rule@(Rule _ r_nt _) _ lookaheads) ->
            if r_nt == Grammar.augment_nt grammar
                then lookaheads & Set.toList & map (\l -> (l, Accept)) & Map.fromList
                else lookaheads & Set.toList & map (\l -> (l, Reduce rule)) & Map.fromList
        )
        grammar
