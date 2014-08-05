import Control.Applicative ((<$>), (<*>))
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (intercalate)
import Prelude hiding (error)

type ParseError = String

type Parser a = ExceptT ParseError (State String) a

runParser :: (Parser a)
          -> String
          -> (Either ParseError a, String)
runParser = runState . runExceptT

char :: Char -> Parser Char
char c = do
    buffer <- get
    case buffer of
        []     -> throwE "Unexpected end of input"
        (x:xs) -> if c == x
                    then do
                      put xs
                      return x 
                    else
                      throwE $ "Expecting "
                            ++ show c
                            ++ ", found "
                            ++ show x

string :: String -> Parser String
string = mapM char

(<|>) :: Parser a -> Parser a -> Parser a
left <|> right = do
    buffer <- get 
    catchE left (\_ -> do
        put buffer
        right)

many :: Parser a -> Parser [a]
many parser = ((:) <$> parser <*> many parser)
          <|> (return [])

manyOne :: Parser a -> Parser [a]
manyOne prs = (:) <$> prs <*> many prs

data Tree = Leaf
          | Node [Tree]
          deriving (Eq, Show)

parens :: Parser Tree
parens = do
    char '('
    inside <- many parens
    char ')'
    if inside == []
        then return Leaf
        else return (Node inside)
