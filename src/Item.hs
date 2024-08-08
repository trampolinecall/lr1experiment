{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}

module Item
    ( Item (..)
    , LR0Item
    , pattern LR0Item
    , LR1Item
    , pattern LR1Item
    , new_lr0
    , new_lr0_with_index_0
    , new_lr1
    , new_lr1_with_index_0
    , lr1_to_lr0
    , lr0_to_lr1
    ) where

import Grammar (Rule, pattern Rule)
import Symbols (Symbol (..), Terminal (..))

class Item i where
    rule :: i -> Rule
    index :: i -> Int

    sym_after_dot :: i -> Maybe Symbol
    can_move_forward :: i -> Bool
    move_forward :: i -> Maybe i

pattern LR0Item :: Rule -> Int -> LR0Item
pattern LR0Item r i <- LR0ItemC r i
{-# COMPLETE LR0Item #-}
data LR0Item = LR0ItemC Rule Int deriving (Show, Eq, Ord)

pattern LR1Item :: Rule -> Int -> Terminal -> LR1Item
pattern LR1Item r i l <- LR1ItemC r i l
{-# COMPLETE LR1Item #-}
data LR1Item = LR1ItemC Rule Int Terminal deriving (Show, Eq, Ord)

new_lr0 :: Rule -> Int -> Maybe LR0Item
new_lr0 r@(Rule _ _ production) i
    | i > length production = Nothing
    | otherwise = Just $ LR0ItemC r i

new_lr0_with_index_0 :: Rule -> LR0Item
new_lr0_with_index_0 r = LR0ItemC r 0

new_lr1 :: Rule -> Int -> Terminal -> Maybe LR1Item
new_lr1 r@(Rule _ _ production) i l
    | i > length production = Nothing
    | otherwise = Just $ LR1ItemC r i l

new_lr1_with_index_0 :: Rule -> Terminal -> LR1Item
new_lr1_with_index_0 r l = LR1ItemC r 0 l

lr1_to_lr0 :: LR1Item -> LR0Item
lr1_to_lr0 (LR1ItemC r i _) = LR0ItemC r i

lr0_to_lr1 :: LR0Item -> Terminal -> LR1Item
lr0_to_lr1 (LR0ItemC r i) lookahead = LR1ItemC r i lookahead

instance Item LR0Item where
    rule (LR0ItemC r _) = r
    index (LR0ItemC _ i) = i

    sym_after_dot (LR0ItemC (Rule _ _ production) index)
        | index < length production = Just (production !! index)
        | otherwise = Nothing

    -- TODO: test that these are consistent with each other
    can_move_forward (LR0ItemC (Rule _ _ r_production) i) = i <= length (r_production)
    move_forward (LR0ItemC r i) = new_lr0 r (i + 1)

instance Item LR1Item where
    rule (LR1ItemC r _ _) = r
    index (LR1ItemC _ i _) = i

    sym_after_dot (LR1ItemC (Rule _ _ production) index _)
        | index < length production = Just (production !! index)
        | otherwise = Nothing

    -- TODO: test that these are consistent with each other
    can_move_forward (LR1ItemC (Rule _ _ r_production) i _) = i <= length (r_production)
    move_forward (LR1ItemC r i l) = new_lr1 r (i + 1) l
