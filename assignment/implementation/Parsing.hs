module Parsing where

import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.Except
import Data.List (intercalate)
import Prelude hiding (error)

type ParseError = String

type Parser a = ExceptT ParseError (State String) a

runP :: Parser a -> String -> (Either ParseError a, String)
runP = runState . runExceptT

char :: Char -> Parser Char
char c = do
    buffer <- get
    case buffer of []     -> throwE "Unexpected end of input"
                   (x:xs) -> if c == x
                                 then do
                                     put xs
                                     return x 
                                 else throwE $ "Expecting " ++ show c ++ ", found " ++ show x
                   
oneOf :: [Char] -> Parser Char
oneOf chars = do
    buffer <- get
    case buffer of []     -> throwE "Unexpected end of input"
                   (x:xs) -> if x `elem` chars then do
                                 put xs
                                 return x
                             else
                                 throwE $ "Expecting one of " ++ (intercalate ", " $ map show chars) ++ "; found " ++ show x

otherThan :: [Char] -> Parser Char
otherThan chars = do
    buffer <- get
    case buffer of []     -> throwE "Unexpected end of input"
                   (x:xs) -> if not $ x `elem` chars then do
                                 put xs
                                 return x
                             else
                                 throwE $ "Expecting one of " ++ (intercalate ", " $ map show chars) ++ "; found " ++ show x

string :: String -> Parser String
string = mapM char


(<|>) :: Parser a -> Parser a -> Parser a
left <|> right = do
    buffer <- get 
    catchE left (\_ -> do
        put buffer
        right)

many :: Parser a -> Parser [a]
many parser = rest parser <|> (return [])
    where rest parser = (:) <$> parser <*> many parser

manyOne :: Parser a -> Parser [a]
manyOne prs = (:) <$> prs <*> many prs

optional :: Parser a -> Parser (Maybe a)
optional prs = do
    buffer <- get
    catchE (Just <$> prs) (\_ -> do
        put buffer
        return Nothing)

token :: Parser a -> Parser a
token prs = do
    many $ oneOf " \t"
    result <- prs
    many $ oneOf " \t"
    return result

stoken :: String -> Parser String
stoken = token . string


