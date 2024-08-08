{-# LANGUAGE PatternSynonyms #-}

module Item
    ( Item
    , pattern Item
    , new
    , new_with_index_0
    , sym_after_dot
    , can_move_forward
    , move_forward
    ) where

import Grammar (Rule, pattern Rule)
import Symbols (Symbol (..), Terminal (..))

pattern Item :: Rule -> Int -> Terminal -> Item
pattern Item r i l <- ItemC r i l
{-# COMPLETE Item #-}
data Item = ItemC Rule Int Terminal deriving (Show, Eq, Ord)

new :: Rule -> Int -> Terminal -> Maybe Item
new r@(Rule _ _ production) i l
    | i > length production = Nothing
    | otherwise = Just $ ItemC r i l

new_with_index_0 :: Rule -> Terminal -> Item
new_with_index_0 r l = ItemC r 0 l

sym_after_dot :: Item -> Maybe Symbol
sym_after_dot (ItemC (Rule _ _ production) index _)
    | index < length production = Just (production !! index)
    | otherwise = Nothing

-- TODO: test that these are consistent with each other
can_move_forward :: Item -> Bool
can_move_forward (ItemC (Rule _ _ r_production) i _) = i <= length (r_production)
move_forward :: Item -> Maybe Item
move_forward (ItemC r i l) = new r (i + 1) l
