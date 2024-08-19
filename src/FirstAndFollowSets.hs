{-# LANGUAGE PatternSynonyms #-}

module FirstAndFollowSets
    ( FirstSet
    , FollowSet
    , FirstSets
    , FollowSets
    , TerminalOrEpsilon (..)
    , find_firsts
    , find_follows
    , firsts_of_sequence
    ) where

import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar as Grammar
import Symbols (NonTerminal, Symbol (..), Terminal (..))
import Utils (repeat_until_unchanging)

type FirstSet = Set TerminalOrEpsilon
type FollowSet = Set Terminal

type FirstSets = Map NonTerminal FirstSet
type FollowSets = Map NonTerminal FollowSet

data TerminalOrEpsilon
    = ToE'Terminal Terminal
    | ToE'Epsilon
    deriving (Show, Eq, Ord)

find_firsts :: Grammar -> FirstSets
find_firsts grammar = repeat_until_unchanging add_firsts Map.empty
    where
        add_firsts firsts =
            Grammar.all_rules grammar
                & map
                    ( \(Rule _ nt production) ->
                        Map.singleton
                            nt
                            ( if null production
                                then Set.singleton ToE'Epsilon
                                else firsts_of_sequence firsts production
                            )
                    )
                & Map.unionsWith (<>)
                & Map.unionWith (<>) firsts

find_follows :: FollowSets -> [Rule] -> FirstSets -> FollowSets
find_follows initial_sets rules first_sets = repeat_until_unchanging add_follows initial_sets
    where
        add_follows follows =
            rules
                & concatMap
                    ( \(Rule _ nt production) ->
                        iter_over_production production
                            & map
                                ( \(current_symbol, after) ->
                                    case current_symbol of
                                        S'NonTerminal current_symbol ->
                                            let firsts_of_after = firsts_of_sequence first_sets after
                                            in Map.singleton current_symbol $
                                                ( firsts_of_after
                                                    & Set.toList
                                                    & mapMaybe
                                                        ( \case
                                                            ToE'Epsilon -> Nothing
                                                            ToE'Terminal t -> Just t
                                                        )
                                                    & Set.fromList
                                                )
                                                    -- if the first set of the sequence after can be empty then this needs to also have the follow set of the rule's nonterminal
                                                    <> (if Set.member ToE'Epsilon firsts_of_after then Map.findWithDefault Set.empty nt follows else Set.empty)
                                        S'Terminal _ -> Map.empty
                                )
                    )
                & Map.unionsWith (<>)
                & Map.unionWith (<>) follows

        iter_over_production [] = []
        iter_over_production (symbol : rest) = (symbol, rest) : iter_over_production rest

firsts_of_sequence :: FirstSets -> [Symbol] -> FirstSet
firsts_of_sequence _ [] = Set.singleton ToE'Epsilon
firsts_of_sequence firsts sequence =
    let sequence_firsts =
            sequence
                & map
                    ( \case
                        S'NonTerminal nt -> Map.findWithDefault Set.empty nt firsts
                        S'Terminal t -> Set.singleton $ ToE'Terminal t
                    )

        (has_epsilon_prefix, after) = span (Set.member ToE'Epsilon) sequence_firsts
    in (Set.unions has_epsilon_prefix)
        -- include the first symbol that does not have epsilon in its first set.
        -- if it does not exist, that means that all of the symbols in the sequence have epsilon in their first set, so the overall first set of the
        -- sequence has to also include epsilon.
        <> (if null after then Set.singleton ToE'Epsilon else head after)
