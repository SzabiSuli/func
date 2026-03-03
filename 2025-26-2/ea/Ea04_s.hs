module Ea04_s where
import Control.Monad

-- Monads

-- examples

-- Calculator

data Op = Add | Sub | Mul | Div
    deriving (Show)

data Expr
    = Lit Int
    | Op Op Expr Expr
    deriving (Show)

example :: Expr
example = Op Add (Lit 3) $ Op Mul (Lit 4) (Lit 5)

example2 :: Expr
example2 = Op Div (Lit 5) (Op Sub (Lit 3) (Lit 3))

applyOp :: Op -> Int -> Int -> Maybe Int
applyOp Add x y = Just $ x + y
applyOp Sub x y = Just $ x - y
applyOp Mul x y = Just $ x * y
applyOp Div x 0 = Nothing
applyOp Div x y = Just $ x `div` y


calc :: Expr -> Maybe Int
calc (Lit x) = Just x
-- calc (Op op x y) = applyOp op (calc x) (calc y)
-- it can have errors

-- calc (Op op e1 e2)
--     | x == Nothing = Nothing
--     | y == Nothing = Nothing
--     | otherwise = applyOp op (fromJust x) (fromJust y)
--     where
--         x = calc e1
        -- y = calc e2

calc (Op op e1 e2) = case calc e1 of 
    Nothing -> Nothing
    Just x -> case calc e2 of
        Nothing -> Nothing
        Just y -> applyOp op x y
-- to many cases


printCalc :: Expr -> String
printCalc x = show (calc x)

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _ = Nothing
andThen (Just a) f = f a


calc' :: Expr -> Maybe Int
calc' (Lit x) = Just x
calc' (Op op e1 e2) = 
    andThen (calc' e1) $ \x -> 
    andThen (calc' e2) $ \y ->
    applyOp op x y

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

recip' :: Rational -> Maybe Rational
recip' x
    | x == 0 = Nothing
    | otherwise = Just (1 / x)

recipTree :: Tree Rational -> Maybe (Tree Rational)
recipTree Leaf = Just Leaf
recipTree (Node l x r) = 
    andThen (recipTree l) $ \l' ->
    andThen (recip' x) $ \x' -> 
    andThen (recipTree r) $ \r' ->
        Just (Node l' x' r')

exampleTree :: Tree Rational
exampleTree = Node (Node Leaf 2 (Node Leaf (1 / 3) Leaf)) 10 (Node Leaf 2.5 Leaf)

exampleTree2 :: Tree String
exampleTree2 = Node (Node Leaf "c" (Node Leaf "c" Leaf)) "a" (Node Leaf "c" Leaf)

-- replace each element wtih an INt, 
-- idetical elements are replaced with the same integer
-- relabel :: Eq a => Tree a -> Tree Int
-- relabel = rh [] where
--     rh :: Eq a => [(a, Int)] -> Tree a -> Tree Int
--     rh _ Leaf = Leaf
--     rh acc (Node l x r) = case lookup x acc of 
--         Nothing -> Node 
--             (rh ((x, length acc) : acc) l) 
--             (length acc)
--             (rh ((x, length acc) : acc) r)
--             -- not good, because there is a seperate acc for the left branch and the right branch

--         Just i -> Node
--             (rh acc l)
--             i
--             (rh acc r)

relabel :: Eq a => Tree a -> Tree Int
relabel x = fst $ rh [] x where
    rh :: Eq a => [(a, Int)] -> Tree a -> (Tree Int, [(a, Int)])
    rh acc Leaf = (Leaf, acc)
    rh acc (Node l x r) =
        let (l', acc1) = rh acc l
            (x', acc2) = case lookup x acc1 of 
                Nothing -> (length acc1, (x, length acc1) : acc1)
                Just n -> (n, acc1)
            (r', acc3) = rh acc2 r
        in (Node l' x' r', acc3)
        

type State s a = s -> (a, s)

andThen' :: State s a -> (a -> State s b) -> State s b
andThen' f g s = 
    let (a, s1) = f s
        (b, s2) = g a s1
    in (b, s2)

noChange :: a -> State s a
noChange a s = (a, s)

get :: State s s 
get s = (s, s)

-- data () = ()

put :: s -> State s ()
put s' s = ((), s')


relabel' :: Eq a => Tree a -> Tree Int
relabel' x = fst $ rh x [] where
    rh :: Eq a => Tree a -> State [(a, Int)] (Tree Int)
    rh Leaf = noChange Leaf
    rh (Node l x r) =
        andThen' (rh l) $ \l' -> 
            andThen' checkElem $ \x' ->
                andThen' (rh r) $ \r' ->
                    noChange (Node l' x' r')
        where 
            checkElem = andThen' get $ \acc -> 
                case lookup x acc of
                    Nothing -> andThen' (put ((x, length acc) : acc)) $ \_ -> 
                        noChange (length acc)
                    Just n -> noChange n


{-

-- actuall Applicative m =>
class Functor m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
    return = Just

    Nothing >>= f = Nothing
    Just x >>= f = f x


-}

newtype State' s a = State' (s -> (a, s))

instance Functor (State' s) where
    fmap f (State' g) = State' $ \s -> 
        let (a, s') = g s 
        in (f a, s')

-- instance Monad (State' s) where
--     return a s = (a, s) 


-- IO
-- instance Monad IO

cat :: IO ()
cat = do
    line <- getLine
    putStrLn line