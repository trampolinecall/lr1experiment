{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module StateTable
    ( StateTable
    , State
    , pattern State
    , GotoTable
    , ActionTable
    , ActionOrConflict (..)
    , Action (..)
    , generate
    , remove_conflicts
    , get_state
    ) where

import Control.Arrow (first, second)
import qualified Control.Monad.Trans.State as StateMonad
import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Text.Layout.Table as Table

import FirstAndFollowSets (find_firsts, find_follows)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar
import ItemAndSet (ItemSet, ItemSetInterner, get_first_item_set, new_item_set, new_item_set_interner, pattern Item)
import qualified ItemAndSet
import Symbols (NonTerminal, Symbol (..), Terminal (..))
import Utils (Display (..))

newtype StateTable conflicts_allowed = StateTable (Map Int (State conflicts_allowed)) deriving Show

pattern State :: Int -> ItemSet -> (ActionTable conflicts_allowed) -> (GotoTable conflicts_allowed) -> (State conflicts_allowed)
pattern State n s a g <- StateC n s a g
{-# COMPLETE State #-}
data State conflicts_allowed = StateC Int ItemSet (ActionTable conflicts_allowed) (GotoTable conflicts_allowed) deriving Show

data Action = Shift Int | Reduce Rule | Accept deriving Show

type ActionTable conflicts_allowed = Map Terminal (ActionOrConflict conflicts_allowed Action)
type GotoTable conflicts_allowed = Map NonTerminal (ActionOrConflict conflicts_allowed Int)

data ActionOrConflict conflicts_allowed a
    = SingleAction a
    | Conflict conflicts_allowed [a]
    deriving Show

data TableConflict
    = ActionConflict [Action] -- TODO: make this better
    | GotoConflict [Int]
    deriving Show

instance Display Action where
    display (Shift next_state) = "s" ++ display next_state
    display (Reduce (Rule rule_num _ _)) = "r" ++ display rule_num
    display Accept = "acc"
instance Display (StateTable conflict_functor) where
    display (StateTable states) =
        Table.tableString $
            Table.columnHeaderTableS
                (Table.defColSpec : (Table.defColSpec <$ all_terminals) ++ (Table.defColSpec <$ all_nonterminals))
                (Table.unicodeTableStyleFromSpec $ Table.simpleTableStyleSpec Table.HeavyLine Table.SingleLine)
                (Table.titlesH $ "number" : map display all_terminals ++ map display all_nonterminals)
                ( states
                    & Map.elems
                    & map
                        ( \(State number _ actions gotos) ->
                            Table.rowG $
                                show number
                                    : map (\term -> maybe "" display_action_or_conflict (Map.lookup term actions)) all_terminals
                                    ++ map (\nt -> maybe "" display_action_or_conflict (Map.lookup nt gotos)) all_nonterminals
                        )
                )
        where
            all_terminals = states & Map.elems & map (\(State _ _ actions _) -> Map.keys actions) & concat & Set.fromList & Set.toAscList
            all_nonterminals = states & Map.elems & map (\(State _ _ _ gotos) -> Map.keys gotos) & concat & Set.fromList & Set.toAscList

            display_action_or_conflict (SingleAction a) = display a
            display_action_or_conflict (Conflict _ as) = intercalate "/" (map display as)

generate :: Grammar -> StateTable ()
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

        go :: Map Int (State ()) -> [ItemSet] -> StateMonad.State ItemSetInterner (StateTable ())
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
                                            ( \(Item rule@(Rule _ r_nt _) _ lookahead) ->
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
                    & fmap (first (Map.unionsWith make_conflict . map (Map.map SingleAction) . concat))
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
                    & fmap (first (Map.unionsWith make_conflict . map (Map.map SingleAction)))
                    & fmap (second concat)

            go
                (Map.insertWith (\_ _ -> error "duplicate state") current_set_number (StateC current_set_number current_set actions gotos) current_table)
                (more_sets ++ new_sets_from_actions ++ new_sets_from_gotos)
        go current_table [] = pure $ StateTable current_table

        make_conflict (SingleAction a) (SingleAction b) = Conflict () ([a, b])
        make_conflict (SingleAction as) (Conflict _ bs) = Conflict () (as : bs)
        make_conflict (Conflict _ as) (SingleAction bs) = Conflict () (bs : as)
        make_conflict (Conflict _ as) (Conflict _ bs) = Conflict () (as ++ bs)

remove_conflicts :: StateTable () -> Either TableConflict (StateTable Void)
remove_conflicts (StateTable states) = StateTable <$> (mapM remove_conflict_from_state states)
    where
        remove_conflict_from_state (StateC number set actions gotos) =
            StateC number set
                <$> mapM
                    ( \case
                        SingleAction a -> Right $ SingleAction a
                        Conflict () conflicts -> Left $ ActionConflict conflicts
                    )
                    actions
                <*> mapM
                    ( \case
                        SingleAction g -> Right $ SingleAction g
                        Conflict () conflicts -> Left $ GotoConflict conflicts
                    )
                    gotos

get_state :: Int -> StateTable conflicts_allowed -> Maybe (State conflicts_allowed)
get_state n (StateTable states) = Map.lookup n states
