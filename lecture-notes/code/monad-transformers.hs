import Control.Monad.Trans
import Control.Monad.Trans.State

type Stack = [Integer]

type StatePlusIO a = StateT Stack IO a

push :: Integer -> StatePlusIO ()
push x = do
    stack <- get
    put (x : stack)
    lift . putStrLn $ "Pushed " ++ show x

pop :: StatePlusIO Integer
pop = do
    (x:xs) <- get
    put xs
    lift . putStrLn $ "Popped " ++ show x
    return x

readPush :: StatePlusIO ()
readPush = do
    lift $ putStr "? "
    x <- lift $ readLn
    push x

add :: StatePlusIO ()
add = do
    x <- pop
    y <- pop
    push (x + y)
    
calculator :: StatePlusIO ()
calculator = do
    readPush
    readPush
    add
