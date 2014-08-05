import Control.Applicative 
import Control.Monad 
import Prelude hiding (Either, Left, Right)

data Either e t = Left e
                | Right t
                deriving (Eq, Show)

instance Functor (Either e) where
    fmap f (Left e) = Left e
    fmap f (Right t) = Right (f t)

instance Applicative (Either e) where
    pure = Right
    Left e <*> _ = Left e
    _ <*> Left e = Left e
    Right f <*> Right t = Right (f t)

instance Monad (Either e) where
    return = Right
    Left e >>= f = Left e
    Right t >>= f = f t
