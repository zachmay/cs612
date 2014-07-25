newtype State s a = State { runState :: s -> (a,s) }  

instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState 


type Stack = [Integer]

pop :: State Stack Integer
pop = State $ \(x:xs) -> (x, xs)

push :: Integer -> State Stack ()
push x = State $ \xs -> ((), x:xs)

add :: State Stack ()
add = pop >>= (\x -> pop >>= \y -> (push (x + y)))

simpleMath :: State Stack Integer
simpleMath = push 2 >>
             push 2 >>
             add >>
             pop

result = runState simpleMath []

add' = do
    x <- pop
    y <- pop
    push (x + y)

simpleMath' = do
    push 2
    push 2
    add
    pop
    
