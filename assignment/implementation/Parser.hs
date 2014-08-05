module Parser where

import AST
import Parsing
import Control.Applicative ((<$>), (<*>))

parseProgram = runP program

program :: Parser Program
program = many statement

statement :: Parser Statement
statement = printStatement
        <|> printStringStatement
        <|> readStatement
        <|> assignStatement
        <|> whileStatement
        <|> ifThenStatement
        <|> ifThenElseStatement
        <|> compoundStatement

printStatement = do
    string "print"
    spaces
    expr <- expression
    spaces
    return $ Print expr

printStringStatement = do
    string "sprint"
    spaces
    str <- stringLiteral
    spaces
    return $ PrintString str
    
readStatement = do
    string "read"
    spaces
    ident <- identifier
    spaces
    return $ Read ident

assignStatement = do
    ident <- identifier
    spaces
    string "="
    spaces
    expr <- expression
    spaces
    return $ Assign ident expr

whileStatement = do
    string "while"
    spaces
    expr <- expression
    spaces
    stmt <- statement
    spaces
    return $ While expr stmt

ifThenStatement = do
    string "if"
    spaces
    expr <- expression
    spaces
    string "then"
    spaces
    trueStmt <- statement
    spaces
    return $ IfThen expr trueStmt

ifThenElseStatement = do
    string "if"
    spaces
    expr <- expression
    spaces
    string "then"
    spaces
    trueStmt <- statement
    spaces
    string "else"
    spaces
    falseStmt <- statement
    spaces
    return $ IfThenElse expr trueStmt falseStmt

compoundStatement = do
    string "{"
    spaces
    stmts <- many statement
    spaces
    string "}"
    spaces
    return $ Compound stmts

{-
expression :: Parser Expression
expression = binaryOpExpr
         <|> unaryOpExpr
         <|> varExpr
         <|> constExpr
-}

expression = do
    t <- term
    spaces
    tail <- optional termTail
    spaces
    case tail of
        Nothing      -> return t
        Just (op, u) -> return $ BinaryOp op t u

termTail = do
    opString <- binaryOperator
    let op = case opString of
                    "+"  -> Add
                    "-"  -> Sub
                    "*"  -> Mul
                    "/"  -> Div
                    "%"  -> Mod
                    "^"  -> Exp
                    "==" -> Equal
                    "!=" -> NotEqual
                    "<=" -> LessThanEq
                    "<"  -> LessThan
                    ">=" -> GreaterThanEq
                    ">"  -> GreaterThan
                    "&&" -> And
                    "||" -> Or
    spaces
    t <- term
    spaces
    return (op, t)
    where binaryOperator = string "+"
                       <|> string "-"
                       <|> string "*"
                       <|> string "/"
                       <|> string "%"
                       <|> string "^"
                       <|> string "=="
                       <|> string "!="
                       <|> string "<="
                       <|> string "<" 
                       <|> string ">="
                       <|> string ">" 
                       <|> string "&&"
                       <|> string "||"

term = varExpr
   <|> constExpr
   <|> unaryOpExpr
   <|> do
           string "("
           expr <- expression
           string ")"
           return expr
             
varExpr = Var <$> identifier

constExpr = Const <$> integerLiteral
    
unaryOpExpr = do
    opString <- unaryOperator
    let op = case opString of
                    "!"  -> LogicalNegate
    spaces
    t <- term
    spaces
    return $ UnaryOp op t
    where unaryOperator = string "!"

{-
unaryOpExpr = fail "Unary operator parsing not implemented"
-}

integerLiteral :: Parser Integer
integerLiteral = do
    negation <- optional $ char '~'
    digits <- manyOne $ oneOf "0123456789"
    let sign = case negation of Nothing -> 1
                                Just _  -> -1
    return $ sign * (read digits)

stringLiteral :: Parser String
stringLiteral = do
    -- Does not yet support escaped quotes
    char '"'
    contents <- many $ otherThan ['"']
    char '"'
    return contents

identifier :: Parser Ident
identifier = do
    first <- alpha
    rest <- many alphanumeric
    return . Ident $ first : rest

alpha :: Parser Char 
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

numeric = oneOf ['0' .. '9']

alphanumeric = alpha <|> numeric

spaces = many $ oneOf " \t\r\n"

