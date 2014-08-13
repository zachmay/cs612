module Parser where

import AST
import Parsing
import Control.Applicative ((<$>), (<*>))

parseProgram = runP program

program :: Parser Program
program = do
    prog <- many statement
    eoi
    return prog

statement :: Parser Statement
statement = printStatement
        <|> printStringStatement
        <|> readStatement
        <|> assignStatement
        <|> whileStatement
        <|> ifThenElseStatement
        <|> ifThenStatement
        <|> compoundStatement

printStatement = do
    strToken "print"
    expr <- expression
    return $ Print expr

printStringStatement = do
    strToken "sprint"
    str <- stringLiteral
    return $ PrintString str
    
readStatement = do
    strToken "read"
    ident <- identifier
    return $ Read ident

assignStatement = do
    ident <- identifier
    strToken "="
    expr <- expression
    return $ Assign ident expr

whileStatement = do
    strToken "while"
    expr <- expression
    stmt <- statement
    return $ While expr stmt

ifThenStatement = do
    strToken "if"
    expr <- expression
    strToken "then"
    trueStmt <- statement
    return $ IfThen expr trueStmt

ifThenElseStatement = do
    strToken "if"
    expr <- expression
    strToken "then"
    trueStmt <- statement
    strToken "else"
    falseStmt <- statement
    return $ IfThenElse expr trueStmt falseStmt

compoundStatement = do
    strToken "{"
    stmts <- many statement
    strToken "}"
    return $ Compound stmts

expression = do
    t <- term
    tail <- optional termTail
    whitespace
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
    t <- term
    return (op, t)
    where binaryOperator = strToken "+"
                       <|> strToken "-"
                       <|> strToken "*"
                       <|> strToken "/"
                       <|> strToken "%"
                       <|> strToken "^"
                       <|> strToken "=="
                       <|> strToken "!="
                       <|> strToken "<="
                       <|> strToken "<" 
                       <|> strToken ">="
                       <|> strToken ">" 
                       <|> strToken "&&"
                       <|> strToken "||"

term = varExpr
   <|> constExpr
   <|> unaryOpExpr
   <|> do
           strToken "("
           expr <- expression
           strToken ")"
           return expr
             
varExpr = Var <$> identifier

constExpr = Const <$> integerLiteral
    
unaryOpExpr = do
    opString <- unaryOperator
    let op = case opString of
                    "!"  -> LogicalNegate
    t <- term
    return $ UnaryOp op t
    where unaryOperator = strToken "!"

integerLiteral :: Parser Integer
integerLiteral = do
    negation <- optional $ char '~'
    digits <- manyOne $ oneOf "0123456789"
    whitespace
    let sign = case negation of Nothing -> 1
                                Just _  -> -1
    return $ sign * (read digits)

stringLiteral :: Parser String
stringLiteral = do
    -- Does not yet support escaped quotes
    char '"'
    contents <- many $ otherThan ['"']
    char '"'
    whitespace
    return contents

identifier :: Parser Ident
identifier = do
    first <- alpha
    rest <- many alphanumeric
    whitespace
    return . Ident $ first : rest

alpha :: Parser Char 
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

numeric = oneOf ['0' .. '9']

alphanumeric = alpha <|> numeric
