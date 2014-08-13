module Parser where

import AST
import Parsing
import Control.Applicative ((<$>), (<*>))

-- | Utility function to parse a string as a program.
--   Partially applies runP from the Parsing module with the
--   the program parser.
parseProgram :: String -> (Either ParseError a, String)
parseProgram = runP program

-- | Top-level parser for a LANG program.
--   Requires parsing to complete with the parse buffer entirely
--   consumed.
program :: Parser Program
program = do
    prog <- many statement
    eoi
    return prog

-- | Parse a single LANG statement.
--   Uses the option parser (<|>) to try each possible statement type.
statement :: Parser Statement
statement = printStatement
        <|> printStringStatement
        <|> readStatement
        <|> assignStatement
        <|> whileStatement
        <|> ifThenElseStatement
        <|> ifThenStatement
        <|> compoundStatement

-- | Print statement parser
--   > print 5 + 5
printStatement = do
    strToken "print"
    expr <- expression
    return $ Print expr

-- | String print statement parser
--   > sprint "hello"
printStringStatement = do
    strToken "sprint"
    str <- stringLiteral
    return $ PrintString str
    
-- | Read statement parser
--   > read var
readStatement = do
    strToken "read"
    ident <- identifier
    return $ Read ident

-- | Assignment statement parser
--   > var = 5 + 5
assignStatement = do
    ident <- identifier
    strToken "="
    expr <- expression
    return $ Assign ident expr

-- | While statement parser
--   > while i < 10
--   >     i = i + 1
whileStatement = do
    strToken "while"
    expr <- expression
    stmt <- statement
    return $ While expr stmt

-- | If-then statement parser
--   > if x < 10 then
--   >     print "Less than"
ifThenStatement = do
    strToken "if"
    expr <- expression
    strToken "then"
    trueStmt <- statement
    return $ IfThen expr trueStmt

-- | If-then-else statement parser
--   > if x < 10 then
--   >     print "Less than"
--   > else
--   >     print "Not less than"
ifThenElseStatement = do
    strToken "if"
    expr <- expression
    strToken "then"
    trueStmt <- statement
    strToken "else"
    falseStmt <- statement
    return $ IfThenElse expr trueStmt falseStmt

-- | Compound statement parser
--   > while i < 10 {
--   >     i = i + 1
--   >     print i
--   > }
compoundStatement = do
    strToken "{"
    stmts <- many statement
    strToken "}"
    return $ Compound stmts

-- | Expression parser
--   An expression consists of a term (a variable reference, a constant,
--   a unary operator expression, or a parenthesized binary operator expression)
--   followed, optionally, by the rest of a binary operator expression. If we
--   fail to parse the tail, we have a simple term and the parse succeeds
--   with that term as the result. Otherwise, we construct the AST value for
--   the binary operator we found.
expression = do
    t <- term
    tail <- optional termTail
    whitespace
    case tail of
        Nothing      -> return t
        Just (op, u) -> return $ BinaryOp op t u

-- | Parse the tail of a binary operator expression.
--   This is just a binary operator symbol followed
--   by another term.
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

-- | Parse a term, i.e., a variable reference or or integer
--   literal, a unary operator application, or a bracketed
--   binary operator expression.
term = varExpr
   <|> constExpr
   <|> unaryOpExpr
   <|> do
           strToken "("
           expr <- expression
           strToken ")"
           return expr
             
-- | Parser for variable reference terms
varExpr = Var <$> identifier

-- | Parser for integer constant terms 
constExpr = Const <$> integerLiteral
    
-- | Parser for unary operator expressions
--   Only supports logical negation.
unaryOpExpr = do
    opString <- unaryOperator
    let op = case opString of
                    "!"  -> LogicalNegate
    t <- term
    return $ UnaryOp op t
    where unaryOperator = strToken "!"

-- | Parser for integer literals with negation using '~'
integerLiteral :: Parser Integer
integerLiteral = do
    negation <- optional $ char '~'
    digits <- manyOne $ oneOf "0123456789"
    whitespace
    let sign = case negation of Nothing -> 1
                                Just _  -> -1
    return $ sign * (read digits)

-- | Parser for string literals without support for escape sequences.
-- Strings can span newlines.
stringLiteral :: Parser String
stringLiteral = do
    char '"'
    contents <- many $ otherThan ['"']
    char '"'
    whitespace
    return contents

-- | Parser for identifiers
identifier :: Parser Ident
identifier = do
    first <- alpha
    rest <- many alphanumeric
    whitespace
    return . Ident $ first : rest

-- | Parser for a single alphabetic character (ASCII)
alpha :: Parser Char 
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

-- | Parser for a single numeric character (ASCII)
numeric :: Parser Char
numeric = oneOf ['0' .. '9']

-- | Parser for a single alphanumeric character (ASCII) in terms of alpha and numeric parsers.
alphanumeric :: Parser Char
alphanumeric = alpha <|> numeric
