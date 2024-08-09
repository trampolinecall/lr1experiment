module Item
    ( Item (..)
    ) where

import Data.Set (Set)

import Grammar (Grammar, Rule)
import Symbols (Symbol (..))

class Item i where
    rule :: i -> Rule
    index :: i -> Int

    sym_after_dot :: i -> Maybe Symbol
    can_move_forward :: i -> Bool
    move_forward :: i -> Maybe i

    find_closure :: Grammar -> Set i -> Set i
