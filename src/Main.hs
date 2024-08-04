{-# LANGUAGE GADTs #-}

module Main (main) where

-- TODO: testing
-- TODO: documenting and comments
-- TODO: unit testing for first and follow sets based on https://www.cs.uaf.edu/~cs331/notes/FirstFollow.pdf

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import qualified Grammar
import Parser (parse)
import qualified StateTable
import Symbols (NonTerminal (..), Symbol (..), Terminal (..))
import Utils (Display (..))

data AnyError where
    AnyError :: Show e => e -> AnyError

instance Show AnyError where
    show (AnyError e) = show e

main :: IO ()
main = do
    result <-
        runExceptT
            ( do
                let augment = NonTerminal "AUGMENT"
                let expr = NonTerminal "E"
                let term = NonTerminal "T"
                let factor = NonTerminal "F"

                let plus = Terminal "+"
                let star = Terminal "*"
                let oparen = Terminal "("
                let cparen = Terminal "")
                let id = Terminal "id"

                grammar <-
                    ExceptT $
                        pure $
                            convert_err $
                                Grammar.make_grammar
                                    [ (augment, [S'NonTerminal expr])
                                    ]
                                    [ (expr, [S'NonTerminal term])
                                    , (expr, [S'NonTerminal expr, S'Terminal plus, S'NonTerminal term])
                                    , (term, [S'NonTerminal factor])
                                    , (term, [S'NonTerminal term, S'Terminal star, S'NonTerminal factor])
                                    , (factor, [S'Terminal oparen, S'NonTerminal expr, S'Terminal cparen])
                                    , (factor, [S'Terminal id])
                                    ]
                let state_table = StateTable.generate grammar
                state_table <- ExceptT $ pure $ convert_err $ StateTable.remove_conflicts state_table

                lift $ putStrLn (display grammar)
                lift $ putStrLn (display state_table)

                parsed <- ExceptT $ pure $ convert_err $ parse state_table ["id", "+", "id", "+", "id", "*", "id"]

                lift $ putStrLn $ display parsed

                pure ()
            )

    case result of
        Right () -> pure ()
        Left err -> putStrLn $ "error: " ++ show err
    where
        convert_err :: Show e => Either e r -> Either AnyError r
        convert_err (Right a) = Right a
        convert_err (Left e) = Left $ AnyError e
