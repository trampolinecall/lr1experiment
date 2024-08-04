{-# LANGUAGE PatternSynonyms #-}

module StateTable
    ( StateTable
    , State
    , GotoTable
    , ActionTable
    , generate
    ) where

import Control.Arrow (first, second)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import qualified Control.Monad.Trans.State as StateMonad

import FirstAndFollowSets (find_firsts, find_follows)
import Grammar (Grammar, Rule (..))
import qualified Grammar
import ItemAndSet (ItemSet, ItemSetInterner, get_first_item_set, new_item_set, new_item_set_interner, pattern Item)
import qualified ItemAndSet
import Symbols (NonTerminal, Symbol (..), Terminal (..))

newtype StateTable = StateTable [State] deriving Show

data State = State Int ItemSet ActionTable GotoTable deriving Show

data Action = Shift Int | Reduce Rule | Accept deriving (Show)

type ActionTable = Map Terminal (Either [Action] Action)
type GotoTable = Map NonTerminal (Either [Int] Int)

generate :: Grammar -> StateTable
generate grammar =
    StateMonad.evalState
        ( do
            first_set <- StateMonad.state $ get_first_item_set grammar follow_sets
            go Map.empty [first_set]
        )
        new_item_set_interner
    where
        first_sets = find_firsts grammar
        follow_sets = find_follows grammar first_sets

        go :: Map Int State -> [ItemSet] -> StateMonad.State ItemSetInterner StateTable
        go current_table (current_set : more_sets) = do
            let current_set_number = ItemAndSet.number current_set
            let symbols_after_dot =
                    ItemAndSet.item_set_items current_set
                        & Set.map (\item -> Map.singleton (ItemAndSet.item_sym_after_dot item) (Set.singleton item))
                        & Map.unionsWith (<>)

            (actions, new_sets_from_actions) <-
                symbols_after_dot
                    & Map.toList
                    & mapM
                        ( \(symbol_after_dot, items) ->
                            case symbol_after_dot of
                                Just (S'Terminal term) -> do
                                    -- fromJust should be safe because the symbol after the dot is a terminal
                                    let new_kernel = Set.map (fromJust . ItemAndSet.item_move_forward) items
                                    (next_set, set_is_new) <- StateMonad.state $ new_item_set (Grammar.all_rules grammar) follow_sets new_kernel

                                    pure ([Map.singleton term (Shift $ ItemAndSet.number next_set)], if set_is_new then [next_set] else [])
                                Nothing ->
                                    pure
                                        ( map
                                            ( \(Item rule@(Rule r_nt _) _ lookahead) ->
                                                if r_nt == Grammar.augment_nt grammar
                                                    then Map.singleton lookahead Accept
                                                    else Map.singleton lookahead (Reduce rule)
                                            )
                                            (Set.toList items)
                                        , []
                                        )
                                _ -> pure ([], [])
                        )
                    & fmap unzip
                    & fmap (first (Map.unionsWith make_conflict . map (Map.map Right) . concat))
                    & fmap (second concat)

            (gotos, new_sets_from_gotos) <-
                symbols_after_dot
                    & Map.toList
                    & mapM
                        ( \(symbol_after_dot, items) ->
                            case symbol_after_dot of
                                Just (S'NonTerminal nt) -> do
                                    let new_kernel = Set.map (fromJust . ItemAndSet.item_move_forward) items
                                    (next_set, set_is_new) <- StateMonad.state $ new_item_set (Grammar.all_rules grammar) follow_sets new_kernel

                                    pure (Map.singleton nt (ItemAndSet.number next_set), if set_is_new then [next_set] else [])
                                _ -> pure (Map.empty, [])
                        )
                    & fmap unzip
                    & fmap (first (Map.unionsWith make_conflict . map (Map.map Right)))
                    & fmap (second concat)

            go
                (Map.insertWith (\_ _ -> error "duplicate state") current_set_number (State current_set_number current_set actions gotos) current_table)
                (more_sets ++ new_sets_from_actions ++ new_sets_from_gotos)
        go current_table [] = pure $ StateTable (Map.elems current_table)

        make_conflict (Right a) (Right b) = Left [a, b]
        make_conflict (Right as) (Left bs) = Left $ as : bs
        make_conflict (Left as) (Right bs) = Left $ bs : as
        make_conflict (Left as) (Left bs) = Left $ as ++ bs
