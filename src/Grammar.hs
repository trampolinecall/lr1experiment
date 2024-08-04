module Grammar
    ( Grammar (all_rules, augment_rules, augment_nt)
    , Rule (..)
    , GrammarConstructionError
    , make_grammar
    ) where

import Data.Function ((&))
import Data.List (intercalate)

import Utils

import Symbols (NonTerminal (..), Symbol)

data Grammar = Grammar
    { all_rules :: [Rule]
    , augment_rules :: [Rule]
    , augment_nt :: NonTerminal
    }

data Rule = Rule NonTerminal [Symbol]
    deriving (Show, Eq, Ord)

instance Display Grammar where
    display (Grammar all_rules _ _) =
        map display all_rules
            & map (++ "\n")
            & concat
instance Display Rule where
    display (Rule nt production) = display nt ++ " -> " ++ intercalate " " (map display production)

data GrammarConstructionError
    = MultipleAugmentNonTerminals [NonTerminal]
    | NoAugmentRules
    deriving Show
make_grammar :: [Rule] -> [Rule] -> Either GrammarConstructionError Grammar
make_grammar augment_rules rest_of_rules
    | null augment_rules = Left NoAugmentRules
    | not $ all ((head augment_nonterminals) ==) augment_nonterminals = Left $ MultipleAugmentNonTerminals augment_nonterminals
    | otherwise = Right $ Grammar (augment_rules ++ rest_of_rules) augment_rules (head augment_nonterminals)
    where
        augment_nonterminals = augment_rules & map (\(Rule nt _) -> nt)
