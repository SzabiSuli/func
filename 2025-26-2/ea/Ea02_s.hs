-- {-# LANGUAGE DataKinds #-} for Type annotation
-- {-# LANGUAGE Somthing forall types#-}
module Ea02_s where

import Data.Kind
import Data.Time (NominalDiffTime)
import Distribution.Compat.Lens (_1)

-- | Bool | = 2

data Three
    = One | Two | Three

-- | Three | = 3

{-
data Either a b
    = Left a
    | Right b
-}

-- | a -> b | = |b| ^ |a|

-- Either a b ~== Either b a -- isomorphism/bijection

{-
f :: a -> b
g :: b -> a
g (f x) = x
f (g x) = x
-}

-- (Bool -> Either a b ~== Either (Bool -> a) (Either (Bool, a, b) (Bool -> b))
-- (|a| + |b|)^|Bool| = |a| ^ |Bool| + |Bool| * |a| * |b| + |b| ^ |Bool|

{-
:k Bool
Bool :: *


-- Type constuctors
:k Either
Either :: * -> * -> *

:k Maybe 
Maybe :: * -> *

[] :: * -> *


instead of * use Type
-}

type Tree :: Type -> Type
data Tree a 
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show)

id' :: forall (a :: Type). a -> a
id' x = x

-- wrong :: Maybe -> Maybe -- kind error
-- wrong = _

-- id'' :: f a -> f a
id'' :: forall (f :: Type -> Type) (a :: Type). f a -> f a
id'' x = x



-- higher-kinded polymorphism
-- not supported by most languages
-- supported by: Haskell, C++, etc..

-- id'' True -> Error Bool != f a
-- id'' [1,2,3] = [1,2,3]
-- [Int] = [] Int


-- 
{-
class Functor (f :: Type -> Type) where
    fmap :: (a -> b) -> f a -> f b
-}

-- map (+1) [1,2,3] == [2,3,4]
-- fmap (+1) [1,2,3] == [2,3,4]


instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- ghci> fmap (+1) $ Node (Leaf 0) (Node (Leaf 1) (Leaf 2))
-- Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

-- instance Functor (Either e) where
-- fmap :: (a -> b) -> Either e a -> Either e b
-- fmap :: (Left e) = Left e
-- fmap :: (Right a) = Right $ f a


-- newtype FunFromInt
data FunFromInt a = FunFromInt (Int -> a)
-- newtype FunFromInt a = FunFromInt (Int -> a)


instance Functor FunFromInt where
    fmap :: (a -> b) -> FunFromInt a -> FunFromInt b
    fmap f (FunFromInt g) = FunFromInt (f . g)


-- not a functor
-- data FunToInt a = FunToInt (a -> Int)
-- instance Functor FunToInt where
--     fmap :: (a -> b) -> FunToInt a -> FunToInt b
--     fmap f (FunToInt g) = FunToInt $ \b -> g _ -- impossible

data Crazy a = Crazy (((Int -> a) -> Int) -> (Int -> a))
--                             ^ co  ^ contr ^ contr ^ covaraiant
--                       -     +      -       -     +
-- because all a-s are in positive positions, this is a Functor
-- In the expression tree the root is + (covariant)
-- and every step left flips its sign
-- Its a functior(covairant) if all a-s are in + places

-- its contravarian if all the a-s are in - places

-- if a-s are in both - and + then its invariant


data Crazy2 b = Crazy2 (((b -> Int) -> Int) -> Int -> b) -- 
--                        -     +       -       -     +
-- invariant


-- how do we know if a type constructor is a functor?

-- type argument can be covariant or contravariant




class Contravariant f where
    contramap :: (b -> a) -> f a -> f b

data Endo a = Endo (a -> a) 
    -- invaraiant functor: (a -> b) -> 


{-
    bivariant
    v       v
covariant contravariant
    v       v
    invariant

-}

-- fixpoint function
fix :: (a -> a) -> a
fix f = f (fix f) -- = f (f (fix f)) = ... = f $ f $ f $ ... 
-- only works because of laziness

ones :: [Int]
ones = fix $ \xs -> 1 : xs

type Fix :: (Type -> Type) -> Type
data Fix f = MkFix (f (Fix f))

data ListF a b -- ~== Maybe (a, b)
    = NilF
    | ConsF a b
    deriving (Functor) -- what? it creates an instance?

type List a = Fix (ListF a) -- MkFix :: ListF a (List a) -> List a

example :: List Int
example = MkFix (ConsF 1 (MkFix (ConsF 2 (MkFix NilF))))

fromFix :: List a -> [a]
fromFix (MkFix NilF) = []
fromFix (MkFix (ConsF x xs)) = x : fromFix xs

type Something = Fix Maybe -- MkFix :: Maybe Something -> Something
type Nat = Fix Maybe -- MkFix :: Maybe Nat -> Nat
-- Zero | Succ Nat
-- Nothing | Just Nat

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f (MkFix x) = f (fmap (fold f) x)

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' f b = fold $ \x -> case x of 
    NilF -> b
    ConsF x r -> f x r

-- instance BiFunctor ...