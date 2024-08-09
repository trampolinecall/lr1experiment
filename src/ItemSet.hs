{-# LANGUAGE PatternSynonyms #-}

module ItemSet
    ( ItemSet (..)
    , new
    , all_items
    ) where

import Data.Set (Set)

import Grammar (Grammar)
import Item (Item)
import qualified Item

data ItemSet item = ItemSet
    { number :: Int
    , kernel :: Set item
    , closure :: Set item
    }
    deriving Show

new :: Item i => Grammar -> Int -> Set i -> ItemSet i
new grammar number kernel = ItemSet number kernel (Item.find_closure grammar kernel)

all_items :: Ord item => ItemSet item -> Set item
all_items (ItemSet _ k c) = k <> c
