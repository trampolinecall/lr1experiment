{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StateTable.Generation (generate) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State as StateMonad
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import Grammar (Grammar, Rule)
import qualified Grammar
import Item (Item)
import qualified Item
import ItemSet (ItemSet)
import qualified ItemSet
import StateTable (Action (..), ActionOrConflict (..), State (..), StateTable)
import qualified StateTable
import Symbols (Symbol (..), Terminal (..))

generate :: forall item. (Ord item, Item item, Show item) => (Rule -> item) -> (item -> Map Terminal Action) -> Grammar -> StateTable item ()
generate make_augment_item make_reduce_actions grammar =
    StateMonad.evalState
        ( do
            (first_set, _) <-
                StateMonad.state $ ItemSet.new grammar (Set.map (\rule -> make_augment_item rule) (Set.fromList $ Grammar.augment_rules grammar))
            go [] [first_set]
        )
        ItemSet.new_interner
    where
        go :: [State item ()] -> [ItemSet item] -> StateMonad.State (ItemSet.Interner item) (StateTable item ())
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
                                    (next_set, set_is_new) <- StateMonad.state $ ItemSet.new grammar new_kernel

                                    pure ([Map.singleton term (Shift $ ItemSet.number next_set)], if set_is_new then [next_set] else [])
                                Nothing -> pure (map make_reduce_actions (Set.toList items), [])
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
                                    (next_set, set_is_new) <- StateMonad.state $ ItemSet.new grammar new_kernel

                                    pure (Map.singleton nt (ItemSet.number next_set), if set_is_new then [next_set] else [])
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
