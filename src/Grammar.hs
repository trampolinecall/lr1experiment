{-# LANGUAGE PatternSynonyms #-}

module Grammar
    ( Grammar (all_rules, augment_rules, augment_nt)
    , Rule
    , pattern Rule
    , GrammarConstructionError
    , make_grammar
    , all_terminals
    ) where

import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Utils

import Symbols (NonTerminal (..), Symbol (..), Terminal (EOF))

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

all_terminals :: Grammar -> Set Terminal
all_terminals (Grammar rules _ _) =
    rules
        & map
            ( \(Rule _ _ prod) ->
                prod
                    & mapMaybe
                        ( \case
                            S'NonTerminal _ -> Nothing
                            S'Terminal t -> Just t
                        )
                    & Set.fromList
            )
        & Set.unions
        & (<> (Set.singleton EOF))
