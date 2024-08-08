{-# LANGUAGE PatternSynonyms #-}

module ItemSet
    ( ItemSet (number, kernel, closure)
    , pattern ItemSet
    , Interner
    , new_interner
    , new_item_set
    , get_first_item_set
    , item_set_items
    ) where

import Data.Function ((&))
import Data.List (find)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import FirstAndFollowSets (FollowSets)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar as Grammar
import Item (Item)
import qualified Item
import Symbols (NonTerminal, Symbol (..))

pattern ItemSet :: Int -> Set Item -> Set Item -> ItemSet
pattern ItemSet n k c <- ItemSetC n k c
{-# COMPLETE ItemSet #-}
data ItemSet = ItemSetC
    { number :: Int
    , kernel :: Set Item
    , closure :: Set Item
    }
    deriving Show

data Interner = Interner Int [ItemSet]

new_interner :: Interner
new_interner = Interner 0 []

new_item_set :: [Rule] -> FollowSets -> Set Item -> Interner -> ((ItemSet, Bool), Interner)
new_item_set rules follow_sets kernel interner@(Interner current_number itemsets) =
    let closure = find_closure rules follow_sets kernel
        found_set = find (\(ItemSetC _ k _) -> k == kernel) itemsets
    in case found_set of
        Just found -> ((found, False), interner)
        Nothing ->
            let new_item_set = ItemSetC current_number kernel closure
            in ((new_item_set, True), Interner (current_number + 1) (new_item_set : itemsets))

get_first_item_set :: Grammar -> FollowSets -> Interner -> (ItemSet, Interner)
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

find_closure :: [Rule] -> FollowSets -> Set Item -> Set Item
find_closure rules follow_sets kernel = go Set.empty (Set.toAscList kernel)
    where
        go current_closure [] = current_closure
        go current_closure (current_item : more) =
            let symbol_after_dot = Item.sym_after_dot current_item
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
                    & Set.map (Item.new_with_index_0 rule)
            )
        & Set.unions
