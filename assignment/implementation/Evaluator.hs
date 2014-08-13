module Evaluator where

import AST
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Map
import Prelude hiding (lookup, read)

-- | Evaluate the given parsed program AST 
evalProgram :: Program -> IO ()
evalProgram program = evalStateT (interpretProgram program) empty

-- | Representation of a program's name scope.
type Env = Map String Integer

-- | Type synonym: the Interpreter monad is the IO monad wrapped in a State monad
--   transformer tracking the programs current name scope. 
type Interpreter = StateT Env IO 

-- | Given a string, load the value of the variable with that name from the
--   name scope if possible. Fails on error.
load :: String -> Interpreter Integer
load ident = do
    env <- get
    case lookup ident env of
        Just x -> return x
        Nothing -> fail $ "Unknown identifier " ++ show ident
       
-- | Save the given integer into the variable with the given name.
store :: String -> Integer -> Interpreter ()
store ident value = modify (insert ident value)

-- | Write the given integer value to standard output
echo :: Integer -> Interpreter ()
echo = liftIO . print 

-- | Read a line from standard input as an integer
read :: Interpreter Integer
read = liftIO $ readLn

-- | Interpret a Program, where a Program is just a list of Statements.
interpretProgram :: Program -> Interpreter ()
interpretProgram = mapM_ interpret

-- | Interpret a Statement.
--       * Empty: The empty statement, just yield ()
--       * Assign: Evaluate the expression and store its value in the given variable.
--       * Print: Evaluate the expression and write the result to standard output. 
--       * PrintString: Print a string literal to the screen.
--       * While: Evaluate the loop condition. If it is non-zero, interpret the body
--         then recursively re-interpret the whole While statement. Otherwise, stop.
--       * If: Evaluate the condition. If the result is non-zero, interpret the
--         true branch statement. Otherwise, interpret the false branch statement.
--       * Compound: Evaluate each constituent statement sequentially.
interpret :: Statement -> Interpreter ()
interpret Empty                        = return ()
interpret (Assign (Ident v) expr)      = evaluate expr >>= store v
interpret (Print expr)                 = evaluate expr >>= echo
interpret (PrintString str)            = liftIO . putStrLn $ str
interpret (Read (Ident v))             = read >>= store v
interpret while@(While expr stmt)      = do
    val <- evaluate expr
    if val /= 0 then do
        interpret stmt
        interpret while 
    else
        return ()
interpret (IfThen expr trueStmt) = do
    val <- evaluate expr
    if val /= 0 then 
        interpret trueStmt
    else
        return ()
interpret (IfThenElse expr trueStmt falseStmt) = do
    val <- evaluate expr
    if val /= 0 then 
        interpret trueStmt
    else
        interpret falseStmt
interpret (Compound stmts) = mapM_ interpret stmts

-- | Evaluate an expression
--       * Const: Yield the constant value
--       * Var: Load and yield the value associated with the identifier
--       * BinaryOp: Evaluate both operands and apply the binary operator to them.
--       * UnaryOp: Evaluate the operand and apply the unary operator to it.
evaluate :: Expression -> Interpreter Integer 
evaluate (Const x)                 = return x
evaluate (Var (Ident ident))       = load ident
evaluate (BinaryOp op expr1 expr2) = evalBinaryOp op <$> evaluate expr1 <*> evaluate expr2
evaluate (UnaryOp op expr)         = evalUnaryOp  op <$> evaluate expr

-- | Apply a pure binary operator to two arguments.
--   Relational operators return 0 for false, 1 for true.
--   Boolean operators consider 0 false and non-zero values true.
evalBinaryOp :: BinaryOperator -> Integer -> Integer -> Integer
evalBinaryOp Add           x y = x + y
evalBinaryOp Sub           x y = x - y
evalBinaryOp Mul           x y = x * y
evalBinaryOp Div           x y = x `div` y -- Integer division
evalBinaryOp Mod           x y = x `mod` y
evalBinaryOp Exp           x y = x ^ y
evalBinaryOp Equal         x y = if x == y then 1 else 0
evalBinaryOp NotEqual      x y = if x == y then 0 else 1
evalBinaryOp GreaterThan   x y = if x > y  then 1 else 0
evalBinaryOp LessThan      x y = if x < y  then 1 else 0
evalBinaryOp GreaterThanEq x y = if x >= y then 1 else 0
evalBinaryOp LessThanEq    x y = if x <= y then 1 else 0
evalBinaryOp And           x y = if x == 0 then 0 else y
evalBinaryOp Or            x y = if x == 0 then y else 1

-- | Apply a pure unary operator to a single argument.
--   Boolean negation uses 0 for false, 1 for true.
evalUnaryOp :: UnaryOperator -> Integer -> Integer
evalUnaryOp LogicalNegate x = if x == 0 then 1 else 0
