{-# LANGUAGE PatternSynonyms #-}

module Parser (AST (..), parse) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Void (Void, absurd)

import Grammar (pattern Rule)
import StateTable (Action (..), ActionOrConflict (..), StateTable, get_state, pattern State)
import Symbols (NonTerminal, Terminal (..))
import Utils (Display (..))

data AST
    = Leaf Terminal
    | Node NonTerminal [AST]
    deriving Show

instance Display AST where
    display (Leaf t) = display t
    display (Node nt children) = "(" ++ display nt ++ concatMap ((" " ++) . display) children ++ ")"

data ParseError = ParseError -- TODO
    deriving Show

parse :: StateTable item Void -> [String] -> Either ParseError [AST]
parse table input = go [] input
    where
        go stack input =
            let (current_state_num, _) = stack_top stack
                (State _ _ actions _) = fromJust $ get_state current_state_num table
                (next_token, more_input) = get_next_token input
                action = Map.lookup next_token actions
            in case action of
                Just (SingleAction (Shift new_state)) -> go ((new_state, Leaf next_token) : stack) more_input
                Just (SingleAction (Reduce (Rule _ nt_just_reduced prod))) ->
                    let prod_length = length prod
                        (popped, stack_with_things_removed) = splitAt prod_length stack
                        new_node = Node nt_just_reduced (map snd (reverse popped))
                        (State _ _ _ prev_state_goto) = fromJust $ get_state (fst $ stack_top stack_with_things_removed) table
                        new_state = Map.lookup nt_just_reduced prev_state_goto
                    in case new_state of
                        Just (SingleAction new_state) -> go ((new_state, new_node) : stack_with_things_removed) input
                        Just (Conflict void _) -> absurd void
                        Nothing -> Left ParseError
                Just (SingleAction Accept) -> Right $ map snd stack
                Nothing -> Left ParseError
                Just (Conflict void _) -> absurd void

        stack_top :: [(Int, AST)] -> (Int, Maybe AST)
        stack_top ((num, ast) : _) = (num, Just ast)
        stack_top [] = (0, Nothing)

        get_next_token [] = (EOF, [])
        get_next_token (tok : more) = (Terminal tok, more)
