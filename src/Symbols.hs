module Symbols
    ( Symbol (..)
    , Terminal (..)
    , NonTerminal (..)
    ) where

import Utils

newtype NonTerminal = NonTerminal String deriving (Show, Eq, Ord)
data Terminal
    = Terminal String
    | EOF
    deriving (Show, Eq, Ord)

data Symbol
    = S'NonTerminal NonTerminal
    | S'Terminal Terminal
    deriving (Show, Eq, Ord)

instance Display NonTerminal where
    display (NonTerminal s) = s
instance Display Terminal where
    display (Terminal s) = s
    display EOF = "EOF"
instance Display Symbol where
    display (S'NonTerminal s) = display s
    display (S'Terminal s) = display s
