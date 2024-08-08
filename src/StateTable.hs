{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module StateTable
    ( StateTable
    , DuplicateState (..)
    , new
    , State (..)
    , GotoTable
    , ActionTable
    , ActionOrConflict (..)
    , Action (..)
    , remove_conflicts
    , get_state
    ) where

import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Text.Layout.Table as Table

import Grammar (Rule, pattern Rule)
import ItemSet (ItemSet)
import Symbols (NonTerminal, Terminal (..))
import Utils (Display (..))

newtype StateTable conflicts_allowed = StateTable (Map Int (State conflicts_allowed)) deriving Show

-- TODO: decide if this is needed
-- pattern State :: Int -> ItemSet -> (ActionTable conflicts_allowed) -> (GotoTable conflicts_allowed) -> (State conflicts_allowed)
-- pattern State n s a g <- StateC n s a g
-- {-# COMPLETE State #-}
data State conflicts_allowed = State Int ItemSet (ActionTable conflicts_allowed) (GotoTable conflicts_allowed) deriving Show

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

data DuplicateState conflicts_allowed = DuplicateState (State conflicts_allowed) (State conflicts_allowed)
new :: [State conflicts_allowed] -> Either (DuplicateState conflicts_allowed) (StateTable conflicts_allowed)
new =
    fmap StateTable
        . foldlM
            ( \state_table state@(State num _ _ _) -> case Map.lookup num state_table of
                Just old_state -> Left $ DuplicateState old_state state
                Nothing -> Right $ Map.insert num state state_table
            )
            Map.empty

remove_conflicts :: StateTable () -> Either TableConflict (StateTable Void)
remove_conflicts (StateTable states) = StateTable <$> (mapM remove_conflict_from_state states)
    where
        remove_conflict_from_state (State number set actions gotos) =
            State number set
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
