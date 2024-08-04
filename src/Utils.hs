module Utils
    ( Display (..)
    , repeat_until_unchanging
    ) where

class Display d where
    display :: d -> String

repeat_until_unchanging :: Eq a => (a -> a) -> a -> a
repeat_until_unchanging change initial =
    let next = change initial
    in if initial == next then next else repeat_until_unchanging change next
