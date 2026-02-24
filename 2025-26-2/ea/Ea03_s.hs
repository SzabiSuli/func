module Ea03_s where

import Data.List
import Data.Monoid (Product(..), All(..))
-- Semigroups, Monoids, foldable
-- these are typeclasses


{-
class Semigroup (m :: Type) where
    (<>) :: m -> m -> m     -- append


instances need to satisfy: 
associativity: (x <> y) <> z == x <> (y <> z)

Haskell doesn't check instances satisfy equations


instance Semigroup [a] where
    xs <> ys == xs ++ ys 


class Semigroup m => Monoid m where
    mempty :: m

needs to satisfy:
    - left unit: mempty <> x == x
    - right unit: x <> mempty == x

instance Monoid [a] where
    mempty = []

-}

mconcat' :: Monoid a => [a] -> a
mconcat' [] = mempty
mconcat' (x:xs) = x <> mconcat' xs

instance Semigroup Int where
    (<>) = (+)
    -- (<>) = (*)

instance Monoid Int where
    mempty = 0
    -- mempty = 1


-- newtype is almost the same as 'data'

-- data Sum = Sum Int
newtype Sum = Sum Int
    deriving (Show)

-- but if we have a single constructor with a single field, we can use newtype
-- more efficient representation
-- the in memory representation is an Int
-- instead of a pointer to an int

instance Semigroup Sum where
    Sum x <> Sum y = Sum $ x + y

instance Monoid Sum where
    mempty = Sum 0

getSum :: Sum -> Int
getSum (Sum x) = x

sum' :: [Int] -> Int
sum' xs = getSum $ mconcat (Sum <$> xs)


newtype Prod = Prod Int
    deriving (Show, Eq)

instance Semigroup Prod where
    Prod x <> Prod y = Prod $ x * y

instance Monoid Prod where
    mempty = Prod 1


-- Semigroup Bool, Monoid Bool
-- (||), False
-- (&&), True
-- xor, False
-- (==), True


-- instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
--     (x1, y1) <> (x2, y2) = (x1 <> x2, y1 <> y2)

-- instance Semigroup b => Semigroup (a -> b) where
--     f <> g = \a -> f a <> g a

-- instance Monoid b => Monoid (a -> b) where
--     mempty = \_ -> mempty

naivePower :: Monoid m => m -> Integer -> m
naivePower x 0 = mempty
naivePower x n = x <> naivePower x (n-1)

power :: Monoid m => m -> Integer -> m
power x 0 = mempty
power x 1 = x
power x n 
    | even n = let res = power x (n `div` 2) in res <> res
    | otherwise = x <> power x (n-1)


fibNaive :: Integer -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n-1) + fibNaive (n-2)


fib :: Integer -> Integer
-- fib 0 = 0
-- fib 1 = 1
fib n = helper n 0 1
    where
        helper 0 curr next = curr 
        helper n curr next = helper (n - 1) next (curr + next)



newtype M22 = M22 [[Integer]]

fibMatrix :: M22
fibMatrix = M22 [[1,1], [1,0]]

instance Semigroup M22 where
    M22 a <> M22 b = M22 [[sum $ zipWith (*) aRow bCol | bCol <- transpose b ] | aRow <- a]

{-

helper 3 0 1
helper 2 1 (0 + 1)
helper 1 (0 + 1) (0 + 1 + 1)
helper 0 (0 + 1 + 1) ((0 + 1) + (0 + 1 + 1))

-}

instance Monoid M22 where
    mempty = M22 [[1,0],[0,1]]

getM22 :: M22 -> [[Integer]]
getM22 (M22 x) = x

fibBest :: Integer -> Integer
fibBest n = getM22 (power fibMatrix n) !! 0 !! 1


{-

class Foldable (f :: Type -> Type) where
    foldMap :: Monoid m => (a -> m) -> f a -> m

    -- generalization of mconcat

instance Foldable [] where
    foldMap :: Monoid m => (a -> m) -> [a] -> m
    foldMap f [] = mempty
    foldMap f (x:xs) = f x <> foldMap f xs

    foldr :: (a -> b -> b) -> b -> f a -> b
    foldl :: (b -> a -> b) -> b -> f a -> b

    when defining a foldable instance, 
    you can either implement foldMap or foldr

-}

sum'' :: [Int] -> Int
sum'' xs = getSum (foldMap Sum xs)

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f Leaf = mempty
    foldMap f (Node xs x ys) = (foldMap f xs) <> (f x) <> (foldMap f ys)
    
example :: Tree Int
example = Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 (Node Leaf 4 Leaf)

sumf :: Foldable f => f Int -> Int
sumf xs = getSum (foldMap Sum xs)

all' :: Foldable f => (a -> Bool) -> f a -> Bool
all' f xs = getAll (foldMap (All . f) xs)

foldMap_ :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap_ f t = foldr (\a m -> f a <> m) mempty t

newtype Endo a = Endo (a -> a) -- endofunction
getEndo :: Endo a -> (a -> a)
getEndo (Endo x) = x 


instance Semigroup (Endo a) where
    Endo f <> Endo g = Endo $ f . g

instance Monoid (Endo a) where
    mempty = Endo id

newtype Dual a = Dual a
instance Semigroup a => Semigroup (Dual a) where
    Dual x <> Dual y = Dual $ y <> x

instance Monoid a => Monoid (Dual a) where
    mempty = Dual mempty

getDual :: Dual a -> a
getDual (Dual a ) = a


-- foldr_ :: Foldable f => (a -> b -> b) -> b -> f a -> b
-- foldr_ f b t = getEndo (getDual (foldMap (\a -> Dual (Endo $ flip f a)) t)) b 