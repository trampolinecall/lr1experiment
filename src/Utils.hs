module Utils
    ( Display (..)
    , repeat_until_unchanging
    ) where

import Data.List (intercalate)

class Display d where
    display :: d -> String

instance Display Int where
    display = show
instance Display a => Display [a] where
    display a = "[" ++ intercalate ", " (map display a) ++ "]"

repeat_until_unchanging :: Eq a => (a -> a) -> a -> a
repeat_until_unchanging change initial =
    let next = change initial
    in if initial == next then next else repeat_until_unchanging change next
