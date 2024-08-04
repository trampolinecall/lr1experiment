{-# LANGUAGE PatternSynonyms #-}

module ItemAndSet
    ( Item
    , pattern Item
    , ItemSet (number, kernel, closure)
    , pattern ItemSet
    , ItemSetInterner
    , new_item_set_interner
    , new_item_set
    , get_first_item_set
    , item_set_items
    , new_item
    , new_item_with_index_0
    , item_sym_after_dot
    , item_can_move_forward
    , item_move_forward
    ) where

import Data.Function ((&))
import Data.List (find)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import FirstAndFollowSets (FollowSets)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar as Grammar
import Symbols (NonTerminal, Symbol (..), Terminal (..))

pattern Item :: Rule -> Int -> Terminal -> Item
pattern Item r i l <- ItemC r i l
{-# COMPLETE Item #-}
data Item = ItemC Rule Int Terminal deriving (Show, Eq, Ord)

pattern ItemSet :: Int -> Set Item -> Set Item -> ItemSet
pattern ItemSet n k c <- ItemSetC n k c
{-# COMPLETE ItemSet #-}
data ItemSet = ItemSetC
    { number :: Int
    , kernel :: Set Item
    , closure :: Set Item
    }
    deriving Show

data ItemSetInterner = ItemSetInterner Int [ItemSet]

new_item_set_interner :: ItemSetInterner
new_item_set_interner = ItemSetInterner 0 []

new_item_set :: [Rule] -> FollowSets -> Set Item -> ItemSetInterner -> ((ItemSet, Bool), ItemSetInterner)
new_item_set rules follow_sets kernel interner@(ItemSetInterner current_number itemsets) =
    let closure = find_closure rules follow_sets kernel
        found_set = find (\(ItemSetC _ k _) -> k == kernel) itemsets
    in case found_set of
        Just found -> ((found, False), interner)
        Nothing ->
            let new_item_set = ItemSetC current_number kernel closure
            in ((new_item_set, True), ItemSetInterner (current_number + 1) (new_item_set : itemsets))

get_first_item_set :: Grammar -> FollowSets -> ItemSetInterner -> (ItemSet, ItemSetInterner)
get_first_item_set grammar follow_sets interner =
    let ((set, _), interner') =
            new_item_set
                (Grammar.all_rules grammar)
                follow_sets
                (make_item_index_0_for_all_rules_with_nt (Grammar.all_rules grammar) follow_sets (Grammar.augment_nt grammar))
                interner
    in (set, interner')

item_set_items :: ItemSet -> Set Item
item_set_items (ItemSetC _ k c) = k <> c

new_item :: Rule -> Int -> Terminal -> Maybe Item
new_item r@(Rule _ _ production) i l
    | i > length production = Nothing
    | otherwise = Just $ ItemC r i l

new_item_with_index_0 :: Rule -> Terminal -> Item
new_item_with_index_0 r l = ItemC r 0 l

item_sym_after_dot :: Item -> Maybe Symbol
item_sym_after_dot (ItemC (Rule _ _ production) index _)
    | index < length production = Just (production !! index)
    | otherwise = Nothing

-- TODO: test that these are consistent with each other
item_can_move_forward :: Item -> Bool
item_can_move_forward (ItemC (Rule _ _ r_production) i _) = i <= length (r_production)
item_move_forward :: Item -> Maybe Item
item_move_forward (ItemC r i l) = new_item r (i + 1) l

find_closure :: [Rule] -> FollowSets -> Set Item -> Set Item
find_closure rules follow_sets kernel = go Set.empty (Set.toAscList kernel)
    where
        go current_closure [] = current_closure
        go current_closure (current_item : more) =
            let symbol_after_dot = item_sym_after_dot current_item
            in case symbol_after_dot of
                Just (S'NonTerminal nt_after_dot) ->
                    let add_to_closure = make_item_index_0_for_all_rules_with_nt rules follow_sets nt_after_dot
                    in go
                        (current_closure <> add_to_closure)
                        (more ++ filter (\i -> not (Set.member i kernel) && not (Set.member i current_closure)) (Set.toAscList add_to_closure))
                _ -> go current_closure more

make_item_index_0_for_all_rules_with_nt :: [Rule] -> FollowSets -> NonTerminal -> Set Item
make_item_index_0_for_all_rules_with_nt rules follow_sets nt =
    rules
        & filter (\(Rule _ r_nt _) -> r_nt == nt)
        & map
            ( \rule@(Rule _ r_nt _) ->
                Map.findWithDefault Set.empty r_nt follow_sets
                    & Set.map (new_item_with_index_0 rule)
            )
        & Set.unions
