module Main where

import Evaluator
import Parser
import Parsing

import Control.Applicative ((<$>))
import System.Environment

-- | Application entry point. 
--   Attempt to load, parse, and execute a program from a file
--   specified on the command line. Print usage message if no
--   file is specified.
main :: IO ()
main = do
    args <- getArgs
    if length args < 1 then do
        putStrLn "Error: No source file specified."
        usage
    else do
        let source = head args
        code <- readFile source
        parseAndRun code
    
-- | Parse code and run it.
parseAndRun :: String -> IO ()
parseAndRun code = do
    case parseProgram code of
        -- Parse failed.
        (Left err, _)    -> error err 
        -- Parse succeeded.
        (Right prog, _) -> evalProgram prog

-- | Print a usage message.
usage :: IO ()
usage = do
    putStrLn "lang - Lang Interpreter"
    putStrLn ""
    putStrLn "Runs the specified Lang source file."
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  lang FILE"
    

