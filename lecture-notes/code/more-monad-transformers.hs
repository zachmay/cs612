import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

type Stack = [Integer]
type Config = [Integer]
type StateReaderIO a = StateT Stack (ReaderT Config IO) a

push :: Integer -> StateReaderIO ()
push x = do
    blacklist <- lift ask
    if x `elem` blacklist then 
        liftIO . putStrLn $ "Forbidden: " ++ show x
    else do
        stack <- get
        put (x : stack)
        liftIO . putStrLn $ "Pushed " ++ show x

pop :: StateReaderIO Integer
pop = do
    (x:xs) <- get
    put xs
    liftIO . putStrLn $ "Popped " ++ show x
    return x

readPush :: StateReaderIO ()
readPush = do
    liftIO $ putStr "? "
    x <- liftIO $ readLn
    push x

add :: StateReaderIO ()
add = do
    x <- pop
    y <- pop
    push (x + y)
    
calculator :: StateReaderIO ()
calculator = do
    readPush
    readPush
    add
