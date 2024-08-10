{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StateTable.Generation (generate, default_intern) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State as StateMonad
import Data.Function ((&))
import qualified Data.List as List (findIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

import Grammar (Grammar, Rule)
import qualified Grammar
import Item (Item)
import qualified Item
import ItemSet (ItemSet (ItemSet))
import qualified ItemSet
import StateTable (Action (..), ActionOrConflict (..), State (..), StateTable)
import qualified StateTable
import Symbols (NonTerminal (..), Symbol (..), Terminal (..))

generate ::
    forall item.
    (Ord item, Item item, Show item) =>
    (Rule -> item) ->
    (Set item -> Set item -> [ItemSet item] -> ((Int, Bool), [ItemSet item])) ->
    (item -> Map Terminal Action) ->
    Grammar ->
    StateTable item ()
generate make_augment_item intern make_reduce_actions grammar =
    let sets =
            StateMonad.evalState
                (find_sets Map.empty [0])
                [ItemSet.new grammar 0 (Set.map (\rule -> make_augment_item rule) (Set.fromList $ Grammar.augment_rules grammar))]
    in convert_to_state_table sets
    where
        find_sets ::
            Map.Map Int (Map.Map Terminal (ActionOrConflict () Int), Map.Map NonTerminal (ActionOrConflict () Int)) ->
            [Int] ->
            StateMonad.State [ItemSet item] ([(ItemSet item, Map.Map Terminal (ActionOrConflict () Int), Map.Map NonTerminal (ActionOrConflict () Int))])
        find_sets item_set_tables (current_set_number : to_process) = do
            current_set <- (!! current_set_number) <$> StateMonad.get

            let symbols_after_dot =
                    ItemSet.all_items current_set
                        & Set.map (\item -> Map.singleton (Item.sym_after_dot item) (Set.singleton item))
                        & Map.unionsWith (<>)

            (shifts, new_sets_from_shifts) <-
                symbols_after_dot
                    & Map.toList
                    & mapM
                        ( \(symbol_after_dot, items) ->
                            case symbol_after_dot of
                                Just (S'Terminal term) -> do
                                    -- fromJust should be safe because the symbol after the dot is a terminal
                                    let new_kernel = Set.map (fromJust . Item.move_forward) items
                                    let closure = Item.find_closure grammar new_kernel
                                    (next_set_number, set_is_new) <- StateMonad.state $ intern new_kernel closure

                                    pure (Map.singleton term next_set_number, if set_is_new then [next_set_number] else [])
                                _ -> pure (Map.empty, [])
                        )
                    & fmap unzip
                    & fmap (first (Map.unionsWith (<>) . map (Map.map SingleAction)))
                    & fmap (second concat)

            (gotos, new_sets_from_gotos) <-
                symbols_after_dot
                    & Map.toList
                    & mapM
                        ( \(symbol_after_dot, items) ->
                            case symbol_after_dot of
                                Just (S'NonTerminal nt) -> do
                                    let new_kernel = Set.map (fromJust . Item.move_forward) items
                                    let closure = Item.find_closure grammar new_kernel
                                    (next_set_number, set_is_new) <- StateMonad.state $ intern new_kernel closure

                                    pure (Map.singleton nt next_set_number, if set_is_new then [next_set_number] else [])
                                _ -> pure (Map.empty, [])
                        )
                    & fmap unzip
                    & fmap (first (Map.unionsWith (<>) . map (Map.map SingleAction)))
                    & fmap (second concat)

            find_sets
                (Map.insert current_set_number (shifts, gotos) item_set_tables)
                (to_process ++ new_sets_from_shifts ++ new_sets_from_gotos)
        find_sets tables [] = do
            sets <- StateMonad.get
            pure $ Map.toAscList tables & map (\(index, (shifts, gotos)) -> (sets !! index, shifts, gotos))

        convert_to_state_table ::
            [(ItemSet item, Map.Map Terminal (ActionOrConflict () Int), Map.Map NonTerminal (ActionOrConflict () Int))] -> StateTable item ()
        convert_to_state_table states =
            case StateTable.new $ map to_state states of
                Right st -> st
                Left (StateTable.DuplicateState s1 s2) -> error $ "duplicate state: " ++ show s1 ++ " " ++ show s2
            where
                to_state :: (ItemSet item, Map Terminal (ActionOrConflict () Int), Map NonTerminal (ActionOrConflict () Int)) -> State item ()
                to_state (item_set, shift_table, goto_table) =
                    State (ItemSet.number item_set) item_set (Map.unionWith (<>) (Map.map (Shift <$>) shift_table) (reduce_actions)) (goto_table)
                    where
                        reduce_actions =
                            ItemSet.all_items item_set
                                & Set.toList
                                & filter (isNothing . Item.sym_after_dot)
                                & map (Map.map SingleAction . make_reduce_actions)
                                & Map.unionsWith (<>)

default_intern :: Ord item => Set item -> Set item -> [ItemSet item] -> ((Int, Bool), [ItemSet item])
default_intern kernel closure sets =
    case List.findIndex (\(ItemSet _ f_kernel f_closure) -> (f_kernel <> f_closure) == (kernel <> closure)) sets of
        Just found_set_index -> ((found_set_index, False), sets)
        Nothing ->
            let new_item_set = ItemSet (length sets) kernel closure
            in (((length sets), True), (sets ++ [new_item_set]))
