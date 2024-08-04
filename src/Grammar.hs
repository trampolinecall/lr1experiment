{-# LANGUAGE PatternSynonyms #-}

module Grammar
    ( Grammar (all_rules, augment_rules, augment_nt)
    , Rule
    , pattern Rule
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

pattern Rule :: Int -> NonTerminal -> [Symbol] -> Rule
pattern Rule n nt prod <- RuleC n nt prod
{-# COMPLETE Rule #-}
data Rule = RuleC Int NonTerminal [Symbol]
    deriving (Show, Eq, Ord)

instance Display Grammar where
    display (Grammar all_rules _ _) =
        map display all_rules
            & map (++ "\n")
            & concat
instance Display Rule where
    display (RuleC num nt production) = display num ++ ": " ++ display nt ++ " -> " ++ intercalate " " (map display production)

data GrammarConstructionError
    = MultipleAugmentNonTerminals [NonTerminal]
    | NoAugmentRules
    deriving Show
make_grammar :: [(NonTerminal, [Symbol])] -> [(NonTerminal, [Symbol])] -> Either GrammarConstructionError Grammar
make_grammar augment_rules rest_of_rules
    | null augment_rules = Left NoAugmentRules
    | not $ all ((head augment_nonterminals) ==) augment_nonterminals = Left $ MultipleAugmentNonTerminals augment_nonterminals
    | otherwise = Right $ Grammar (augment_rules_converted ++ rest_of_rules_converted) augment_rules_converted (head augment_nonterminals)
    where
        augment_rules_converted = zipWith (\i (nt, prod) -> RuleC i nt prod) [0 ..] augment_rules
        rest_of_rules_converted = zipWith (\i (nt, prod) -> RuleC i nt prod) [length augment_rules ..] rest_of_rules

        augment_nonterminals = augment_rules_converted & map (\(Rule _ nt _) -> nt)
