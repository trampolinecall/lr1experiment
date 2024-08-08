{-# LANGUAGE PatternSynonyms #-}

module ItemSet
    ( ItemSet (number, kernel, closure)
    , pattern ItemSet
    -- , Interner
    -- , new_interner
    , new_item_set_lr0
    , new_item_set_lr1
    , new_item_set_lr1_with_follows
    , augment_items
    , item_set_items
    ) where

import Data.Function ((&))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import FirstAndFollowSets (FirstSets, FollowSets, find_follows)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar as Grammar
import Item (LR0Item, LR1Item, pattern LR0Item, pattern LR1Item)
import qualified Item
import Symbols (NonTerminal, Symbol (S'NonTerminal))

pattern ItemSet :: Int -> Set item -> Set item -> ItemSet item
pattern ItemSet n k c <- ItemSetC n k c
{-# COMPLETE ItemSet #-}
data ItemSet item = ItemSetC
    { number :: Int
    , kernel :: Set item
    , closure :: Set item
    }
    deriving Show

-- TODO: only LALR uses interning / merging item sets
-- data Interner item = Interner Int [ItemSet item]
--
-- new_interner :: Interner item
-- new_interner = Interner 0 []

new_item_set_lr0 :: [Rule] -> Int -> Set LR0Item -> ItemSet LR0Item
new_item_set_lr0 rules number kernel =
    let closure = find_closure_lr0 rules kernel
    in ItemSetC number kernel closure

new_item_set_lr1 :: FirstSets -> [Rule] -> Int -> Set LR1Item -> ItemSet LR1Item
new_item_set_lr1 first_sets rules number kernel =
    let closure = find_closure_lr1 (Left first_sets) rules kernel
    in ItemSetC number kernel closure

new_item_set_lr1_with_follows :: FollowSets -> [Rule] -> Int -> Set LR1Item -> ItemSet LR1Item
new_item_set_lr1_with_follows follows rules number kernel =
    let closure = find_closure_lr1 (Right follows) rules kernel
    in ItemSetC number kernel closure

item_set_items :: Ord item => ItemSet item -> Set item
item_set_items (ItemSetC _ k c) = k <> c

augment_items :: Grammar -> Set LR0Item
augment_items grammar = make_item_index_0_for_all_rules_with_nt (Grammar.all_rules grammar) (Grammar.augment_nt grammar)

find_closure_lr0 :: [Rule] -> Set LR0Item -> Set LR0Item
find_closure_lr0 rules kernel = go Set.empty (Set.toAscList kernel)
    where
        go current_closure [] = current_closure
        go current_closure (current_item : more) =
            let symbol_after_dot = Item.sym_after_dot current_item
            in case symbol_after_dot of
                Just (S'NonTerminal nt_after_dot) ->
                    let to_add_to_closure = make_item_index_0_for_all_rules_with_nt rules nt_after_dot
                    in go
                        (current_closure <> to_add_to_closure)
                        (more ++ filter (\i -> not (Set.member i kernel) && not (Set.member i current_closure)) (Set.toAscList to_add_to_closure))
                _ -> go current_closure more

find_closure_lr1 :: Either FirstSets FollowSets -> [Rule] -> Set LR1Item -> Set LR1Item
find_closure_lr1 follow_sets_or_first_sets rules kernel = Set.unions $ Set.map lr0_to_lr1 lr0_closure
    where
        lr0_closure = find_closure_lr0 rules (Set.map Item.lr1_to_lr0 kernel)

        follow_sets = case follow_sets_or_first_sets of
            Right follow_sets -> follow_sets
            Left first_sets ->
                find_follows
                    ( kernel
                        & Set.map (\(LR1Item (Rule _ nt _) _ lookahead) -> Map.singleton nt (Set.singleton lookahead))
                        & Map.unionsWith (<>)
                    )
                    ((kernel & Set.map Item.rule & Set.toList) <> (lr0_closure & Set.map Item.rule & Set.toList))
                    first_sets

        lr0_to_lr1 lr0_item@(LR0Item (Rule _ nt _) _) = Set.map (Item.lr0_to_lr1 lr0_item) (follow_sets Map.! nt)

make_item_index_0_for_all_rules_with_nt :: [Rule] -> NonTerminal -> Set LR0Item
make_item_index_0_for_all_rules_with_nt rules nt =
    rules
        & filter (\(Rule _ r_nt _) -> r_nt == nt)
        & map (\rule -> Item.new_lr0_with_index_0 rule)
        & Set.fromList
