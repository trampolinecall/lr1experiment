{-# LANGUAGE GADTs #-}

module Main (main) where

-- TODO: testing
-- TODO: unit test that each type of state generation can handle those grammars (eg test that lalr can handle an lalr grammar but not an lr(1) grammar, ...)
-- TODO: documenting and comments
-- TODO: unit testing for first and follow sets based on https://www.cs.uaf.edu/~cs331/notes/FirstFollow.pdf

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import qualified Grammar
import qualified LALR
import qualified LR0
import qualified LR1
import Parser (parse)
import qualified SLR
import qualified StateTable
import Symbols (NonTerminal (..), Symbol (..), Terminal (..))
import Utils (Display (..))

data AnyError where
    AnyError :: Show e => e -> AnyError

instance Show AnyError where
    show (AnyError e) = show e

print_except_t_result :: Show err => ExceptT err IO () -> IO ()
print_except_t_result thing = do
    result <- runExceptT thing

    case result of
        Right () -> pure ()
        Left err -> putStrLn $ "error: " ++ show err

convert_err :: Show e => Either e r -> Either AnyError r
convert_err (Right a) = Right a
convert_err (Left e) = Left $ AnyError e

main :: IO ()
main =
    print_except_t_result $
        do
            -- let augment = NonTerminal "AUGMENT"
            -- let expr = NonTerminal "E"
            -- let term = NonTerminal "T"
            -- let factor = NonTerminal "F"
            -- let plus = Terminal "+"
            -- let star = Terminal "*"
            -- let oparen = Terminal "("
            -- let cparen = Terminal ")"
            -- let id = Terminal "id"
            -- grammar <-
            --     ExceptT $
            --         pure $
            --             convert_err $
            --                 Grammar.make_grammar
            --                     [ (augment, [S'NonTerminal expr])
            --                     ]
            --                     [ (expr, [S'NonTerminal term])
            --                     , (expr, [S'NonTerminal expr, S'Terminal plus, S'NonTerminal term])
            --                     , (term, [S'NonTerminal factor])
            --                     , (term, [S'NonTerminal term, S'Terminal star, S'NonTerminal factor])
            --                     , (factor, [S'Terminal oparen, S'NonTerminal expr, S'Terminal cparen])
            --                     , (factor, [S'Terminal id])
            --                     ]

            let json = NonTerminal "json"
            let value = NonTerminal "value"
            let object = NonTerminal "object"
            let members = NonTerminal "members"
            let member = NonTerminal "member"
            let null = Terminal "null"
            let obrace = Terminal "{"
            let cbrace = Terminal "}"
            let comma = Terminal ","
            let colon = Terminal ":"
            let string = Terminal "STRING"
            grammar <-
                ExceptT $
                    pure $
                        convert_err $
                            Grammar.make_grammar
                                [ (json, [S'NonTerminal value])
                                ]
                                [ (value, [S'NonTerminal object])
                                , (value, [S'Terminal null])
                                , (object, [S'Terminal obrace, S'NonTerminal members, S'Terminal cbrace])
                                , (members, [S'NonTerminal members, S'Terminal comma, S'NonTerminal member])
                                , (members, [S'NonTerminal member])
                                , (member, [S'Terminal string, S'Terminal colon, S'NonTerminal value])
                                ]

            -- let ntS = NonTerminal "S"
            -- let ntE = NonTerminal "E"
            -- let ntF = NonTerminal "F"
            -- let ta = Terminal "a"
            -- let tb = Terminal "b"
            -- let tc = Terminal "c"
            -- let td = Terminal "d"
            -- let te = Terminal "e"
            --
            -- grammar <-
            --     ExceptT $
            --         pure $
            --             convert_err $
            --                 Grammar.make_grammar
            --                     [ (ntS, [S'Terminal ta, S'NonTerminal ntE, S'Terminal tc])
            --                     , (ntS, [S'Terminal ta, S'NonTerminal ntF, S'Terminal td])
            --                     , (ntS, [S'Terminal tb, S'NonTerminal ntF, S'Terminal tc])
            --                     , (ntS, [S'Terminal tb, S'NonTerminal ntE, S'Terminal td])
            --                     ]
            --                     [ (ntE, [S'Terminal te])
            --                     , (ntF, [S'Terminal te])
            --                     ]

            lift $ putStrLn (display grammar)

            let test_generator :: String -> (Grammar.Grammar -> StateTable.StateTable item ()) -> ExceptT e IO ()
                test_generator name generate =
                    lift $ print_except_t_result $ do
                        lift $ putStrLn $ name ++ ":"

                        let state_table = generate grammar
                        state_table <- ExceptT $ pure $ convert_err $ StateTable.remove_conflicts state_table

                        lift $ putStrLn (display state_table)

                        parsed <- ExceptT $ pure $ convert_err $ parse state_table ["id", "+", "id", "+", "id", "*", "id"]
                        lift $ putStrLn $ display parsed

            test_generator "LR(0)" LR0.generate
            test_generator "SLR" SLR.generate
            test_generator "LR(1)" LR1.generate
            test_generator "LALR" LALR.generate

            pure ()
