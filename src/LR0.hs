{-# LANGUAGE PatternSynonyms #-}

module LR0 (generate) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State as StateMonad
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import Grammar (Grammar, pattern Rule)
import qualified Grammar
import Item (LR0Item, pattern LR0Item)
import qualified Item
import ItemSet (ItemSet, augment_items, new_item_set_lr0)
import qualified ItemSet
import StateTable (Action (..), ActionOrConflict (..), State (..), StateTable)
import qualified StateTable
import Symbols (Symbol (..))

generate :: Grammar -> StateTable LR0Item ()
generate grammar =
    let first_set = new_item_set_lr0 (Grammar.all_rules grammar) 0 (augment_items grammar)
    in StateMonad.evalState (go [] [first_set]) 1
    where
        go :: [State LR0Item ()] -> [ItemSet LR0Item] -> StateMonad.State Int (StateTable LR0Item ())
        go states (current_set : more_sets) = do
            let current_set_number = ItemSet.number current_set
            let symbols_after_dot =
                    ItemSet.item_set_items current_set
                        & Set.map (\item -> Map.singleton (Item.sym_after_dot item) (Set.singleton item))
                        & Map.unionsWith (<>)

            (actions, new_sets_from_actions) <-
                symbols_after_dot
                    & Map.toList
                    & mapM
                        ( \(symbol_after_dot, items) ->
                            case symbol_after_dot of
                                Just (S'Terminal term) -> do
                                    -- fromJust should be safe because the symbol after the dot is a terminal
                                    let new_kernel = Set.map (fromJust . Item.move_forward) items
                                    set_number <- get_state_number_and_inc
                                    let next_set = new_item_set_lr0 (Grammar.all_rules grammar) set_number new_kernel

                                    pure ([Map.singleton term (Shift $ ItemSet.number next_set)], [next_set])
                                Nothing ->
                                    pure
                                        ( map
                                            ( \(LR0Item rule@(Rule _ r_nt _) _) ->
                                                let action = if r_nt == Grammar.augment_nt grammar then Accept else Reduce rule
                                                in Grammar.all_terminals grammar & Set.toList & map (\term -> (term, action)) & Map.fromList
                                            )
                                            (Set.toList items)
                                        , []
                                        )
                                _ -> pure ([], [])
                        )
                    & fmap unzip
                    & fmap (first (Map.unionsWith (<>) . map (Map.map SingleAction) . concat))
                    & fmap (second concat)

            (gotos, new_sets_from_gotos) <-
                symbols_after_dot
                    & Map.toList
                    & mapM
                        ( \(symbol_after_dot, items) ->
                            case symbol_after_dot of
                                Just (S'NonTerminal nt) -> do
                                    let new_kernel = Set.map (fromJust . Item.move_forward) items
                                    set_number <- get_state_number_and_inc
                                    let next_set = new_item_set_lr0 (Grammar.all_rules grammar) set_number new_kernel

                                    pure (Map.singleton nt (ItemSet.number next_set), [next_set])
                                _ -> pure (Map.empty, [])
                        )
                    & fmap unzip
                    & fmap (first (Map.unionsWith (<>) . map (Map.map SingleAction)))
                    & fmap (second concat)

            go
                (State current_set_number current_set actions gotos : states)
                (more_sets ++ new_sets_from_actions ++ new_sets_from_gotos)
        go states [] =
            case StateTable.new states of
                Right st -> pure st
                Left (StateTable.DuplicateState s1 s2) -> error $ "duplicate state: " ++ show s1 ++ " " ++ show s2

        get_state_number_and_inc = do
            n <- StateMonad.get
            StateMonad.put (n + 1)
            pure n
