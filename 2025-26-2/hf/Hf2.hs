module Hf2 where 

import Data.Functor.Classes -- Ezt a sort rakd a fájl legelejére
import Data.Kind


data Tree a = Leaf (Maybe a) | Node (Tree a) a (Tree a) deriving (Eq, Show)
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf mb) = Leaf $ fmap f mb -- use fmap of Maybe
    fmap f (Node x y z) = Node (fmap f x) (f y) (fmap f z)

type Gofri :: (Type -> Type) -> Type -> Type
data Gofri f a
    = MkGofri (f a) (f (Gofri f a))

deriving instance (Eq a, Eq1 f) => Eq (Gofri f a)
deriving instance (Show a, Show1 f) => Show (Gofri f a)

instance (Functor f) => Functor (Gofri f) where
    fmap :: (Functor f) => (a -> b) -> Gofri f a -> Gofri f b
    fmap m (MkGofri fa x) = MkGofri (fmap m fa) (fmap (fmap m) x)
--                                   ^^^^        ^^^^  ^^^^
--                                   fmap of f   |
--                                               fmap of f
--                                                     fmap of Gofri f (recursive call)  

data CrazyType3 a b
    = CrazyCon1 a b a 
    | CrazyCon2 (CrazyType3 a b) [b] [a]
    | CrazyCon3 (CrazyType3 Int b) (CrazyType3 a a) [[b]]
    deriving (Eq, Show)

instance Functor (CrazyType3 a) where
    fmap :: (b -> c) -> CrazyType3 a b -> CrazyType3 a c
    fmap f (CrazyCon1 a b c) = CrazyCon1 a (f b) c
    fmap f (CrazyCon2 x y z) = CrazyCon2 (fmap f x) (fmap f y) z
    fmap f (CrazyCon3 x y z) = CrazyCon3 (fmap f x) y (fmap (fmap f) z)
--                                                  on [[]]  on []
