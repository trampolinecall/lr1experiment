{-# LANGUAGE PatternSynonyms #-}

module ItemSet
    ( ItemSet (number, kernel, closure)
    , pattern ItemSet
    , Interner
    , new_interner
    , new

    , item_set_items
    ) where

import Data.Function ((&))
import qualified Data.List as List (find)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import FirstAndFollowSets (FirstSets, FollowSets, find_follows)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar as Grammar
import Item (Item)
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

data Interner item = Interner Int [ItemSet item]

new_interner :: Interner item
new_interner = Interner 0 []

intern :: (Ord item, Item item) => Set item -> Set item -> Interner item -> ((ItemSet item, Bool), Interner item)
intern kernel closure interner@(Interner number sets) =
    case List.find (\(ItemSetC _ f_kernel f_closure) -> Item.sets_equal (f_kernel <> f_closure) (kernel <> closure)) sets of
        Just found_set -> ((found_set, False), interner)
        Nothing ->
            let new_item_set = ItemSetC number kernel closure
            in ((new_item_set, True), Interner (number + 1) (new_item_set : sets))

new :: (Ord i, Item i) => Grammar -> Set i -> Interner i -> ((ItemSet i, Bool), Interner i)
new grammar kernel interner =
    let closure = Item.find_closure grammar kernel
    in intern kernel closure interner

item_set_items :: Ord item => ItemSet item -> Set item
item_set_items (ItemSetC _ k c) = k <> c
