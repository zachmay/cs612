module Parsing where

import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.Except
import Data.Char
import Data.List (intercalate)
import Prelude hiding (error)

{- ParseError is a synonym for String -}
type ParseError = String

{- A Parser is a stateful computation where the state is the current
 - parse buffer (a String) that results in either a parsed value
 - or an error of type ParseError. -}
type Parser a = ExceptT ParseError (State String) a

{- Run a parser on the given string. -}
runP :: Parser a -> String -> (Either ParseError a, String)
runP = runState . runExceptT

{- Parse a single character -}
char :: Char -> Parser Char
char c = do
    buffer <- get
    case buffer of []     -> throwE "Unexpected end of input"
                   (x:xs) -> if c == x
                                 then do
                                     put xs
                                     return x 
                                 else throwE $ "Expecting " ++ show c ++ ", found " ++ show x


                   
{- Parse end of input, consumes no input. -}
eoi :: Parser ()
eoi = do
    buffer <- get
    case buffer of []     -> return ()
                   (x:xs) -> throwE $ "Expecting end of input, found " ++ show x

{- Parse a single character if it satisfies the given predicate. -}
satisfies :: (Char -> Bool) -> Parser Char
satisfies pred = do
    buffer <- get
    case buffer of []     -> throwE $ "Unexpected end of input"
                   (x:xs) -> if pred x then do
                                 put xs
                                 return x
                             else
                                 throwE $ "Found " ++ show x ++ " which does not satisfy given predicate."

{- Parse a single character if it is included in the given list of characters. -}
oneOf :: [Char] -> Parser Char
oneOf chars = do
    buffer <- get
    case buffer of []     -> throwE "Unexpected end of input"
                   (x:xs) -> if x `elem` chars then do
                                 put xs
                                 return x
                             else
                                 throwE $ "Expecting one of " ++ (intercalate ", " $ map show chars) ++ "; found " ++ show x

{- Parse a single character if it is not included in the given list of characters. -}
otherThan :: [Char] -> Parser Char
otherThan chars = do
    buffer <- get
    case buffer of []     -> throwE "Unexpected end of input"
                   (x:xs) -> if not $ x `elem` chars then do
                                 put xs
                                 return x
                             else
                                 throwE $ "Expecting one of " ++ (intercalate ", " $ map show chars) ++ "; found " ++ show x

{- Parse the given String. -}
string :: String -> Parser String
string = mapM char

{- The option parser. Attempts to parse the left-hand parser. If it fails, restore
 - the buffer and attempt to parse the right-hand parser. -}
(<|>) :: Parser a -> Parser a -> Parser a
left <|> right = do
    buffer <- get 
    catchE left (\_ -> do
        put buffer
        right)

{- Apply the given parser zero or more times, returning the results as a list. -}
many :: Parser a -> Parser [a]
many parser = rest parser <|> (return [])
    where rest parser = (:) <$> parser <*> many parser

{- Apply the given parser one or more times, returning the results as a list. -}
manyOne :: Parser a -> Parser [a]
manyOne prs = (:) <$> prs <*> many prs

{- Attempt to apply the given parser.
 - If it fails, restore the buffer and return Nothing. If it succeeds,
 - return Just the result. -}
optional :: Parser a -> Parser (Maybe a)
optional prs = do
    buffer <- get
    catchE (Just <$> prs) (\_ -> do
        put buffer
        return Nothing)

{- Apply the given parser, then consume any white space. -}
token :: Parser a -> Parser a
token prs = do
    result <- prs
    many $ satisfies isSpace
    return result

strToken :: String -> Parser String
strToken str = token (string str)

whitespace :: Parser ()
whitespace = do
    many (satisfies isSpace)
    return ()
