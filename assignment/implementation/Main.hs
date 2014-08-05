module Main where

import Evaluator
import Parser
import Parsing

parseAndRun :: String -> IO ()
parseAndRun code = runProgram prog >> return ()
    where (Right prog, _) = parseProgram code
