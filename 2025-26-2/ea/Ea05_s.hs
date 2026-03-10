module Ea05 where

import Control.Monad.State
import Distribution.System (OS)
import Control.Applicative (Alternative(empty))
-- import NonDet

{-
what should the rule be for fmap?

instance Functor f where
    fmap :: (a -> b) -> f a -> f b

Functor laws:
    fmap id x == x
    fmap f (fmap g x) == fmap (f . g) x

    optimizations


-- Monads ~ (side) effects
class Functor m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

newtype State s a = State (s -> (a, s))

instance Functor (State s)
    ...

instance Monad (State s) where
    return a = State $ \s -> (a, s)

    State f >>= g = State $ \s -> 
        let (a, s') = f s
        in runState (g a) s'

runState :: State s a -> s -> (a, s)

Monad laws:
    -- left unit:
    (return x >>= f) == f x

    -- right unit
    (m >>= \x -> return x) == m

    -- associativity:
    ((m >>= f) >>= g) == (m >>= \x -> (f x >>= g))



get :: State s s
put :: s -> State s ()
-}


--cumsum [1,2,3,4] == [1,3,6,10]
cumsum :: [Int] -> [Int]
cumsum xs = fst $ runState (helper xs) 0 where
    helper :: [Int] -> State Int [Int]
    helper [] = return []
    helper (x:xs) = 
        get >>= \oldSum -> -- \s -> (s, s) 
        let newSum = oldSum + x in
        put newSum >>
        helper xs >>= \xs' -> --
        return $ newSum : xs' -- \s -> (newSum : xs', s)


mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = 
    f x >>= \x' ->
    mapM' f xs >>= \xs' ->
    return $ x' : xs'

cumsum' :: [Int] -> [Int]
cumsum' xs = fst $ runState (mapM' helper xs) 0
    where
        helper x = 
            get >>= \oldSum ->
            let newSum = oldSum + x in
            put newSum >>
            return newSum


replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m
    | n <= 0 = return []
    | otherwise =
        m >>= \x ->
        replicateM (n - 1) m >>= \xs ->
        return $ x : xs


while :: Monad m => m Bool -> m () -> m ()
while cond body = 
    cond >>= \b ->
    if b 
        then 
            body >>
            while cond body
        else return ()


-- List Monad

{-
instance Monad [] where
    return :: a -> [a]
    return x = [x]


    (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = [y | x <- xs, y <- f x]

-}

type NonDet = []


data Flip = Heads | Tails
    deriving (Show)


flipCoin :: NonDet Flip
flipCoin = [Heads, Tails]

flipTwice' :: NonDet (Flip, Flip)
flipTwice' = [(x, y) | x <- flipCoin, y <- flipCoin]


-- List Monad = List Comprehension


-- get all teh end states of tic-tac-toe

data XO = X | O
    deriving (Show, Eq)

type Board = [[Maybe XO]]

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)



xMove :: Board -> NonDet Board
xMove = do
    row <- [0..2]
    column <- [0..2]
    guard $ _ -- check the (row, col) is free
    _ -- update board

checkWin :: Board -> Bool
checkWin = _

endStates :: NonDet Board
endStates = helper empty
    where
        helper board
            | checkWin board = return board
            | checkTie board = return board
            | otherwise = do
                board' <- xMove
                helperO board'