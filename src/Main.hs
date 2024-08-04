-- TODO: testing
-- TODO: documenting and comments
-- TODO: unit testing for first and follow sets based on https://www.cs.uaf.edu/~cs331/notes/FirstFollow.pdf

module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import qualified Grammar
import qualified StateTable
import Symbols (NonTerminal (..), Symbol (..), Terminal (..))
import Utils (Display (..))

main :: IO ()
main = do
    result <-
        runExceptT
            ( do
                let augment = NonTerminal ("AUGMENT")
                let expr = NonTerminal ("E")
                let expr' = NonTerminal ("E'")
                let term = NonTerminal ("T")
                let term' = NonTerminal ("T'")
                let factor = NonTerminal ("F")

                let plus = Terminal ("+")
                let star = Terminal ("*")
                let oparen = Terminal ("(")
                let cparen = Terminal (")")
                let id = Terminal ("id")

                grammar <-
                    ExceptT $
                        pure $
                            Grammar.make_grammar
                                [ (augment, [S'NonTerminal expr])
                                ]
                                [ (expr, [S'NonTerminal term, S'NonTerminal expr'])
                                , (expr', [S'Terminal (plus), S'NonTerminal (term), S'NonTerminal (expr')])
                                , (expr', [])
                                , (term, [S'NonTerminal (factor), S'NonTerminal (term')])
                                , (term', [S'Terminal (star), S'NonTerminal (factor), S'NonTerminal (term')])
                                , (term', [])
                                , (factor, [S'Terminal (oparen), S'NonTerminal (expr), S'Terminal (cparen)])
                                , (factor, [S'Terminal (id)])
                                ]
                let state_table = StateTable.generate grammar

                lift $ putStrLn (display grammar)
                lift $ putStrLn (display state_table)
                pure ()
            )

    case result of
        Right () -> pure ()
        Left err -> putStrLn $ "error: " ++ show err
