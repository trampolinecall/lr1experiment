{-# LANGUAGE PatternSynonyms #-}

module ItemSet
    ( ItemSet (..)
    , new
    , all_items
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

data ItemSet item = ItemSet
    { number :: Int
    , kernel :: Set item
    , closure :: Set item
    }
    deriving Show

new :: (Ord i, Item i) => Grammar -> Int -> Set i -> ItemSet i
new grammar number kernel = ItemSet number kernel (Item.find_closure grammar kernel)

all_items :: Ord item => ItemSet item -> Set item
all_items (ItemSet _ k c) = k <> c
