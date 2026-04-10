module Hf8 where

import Data.Functor.Classes -- Ezt a sort rakd a fájl legelejére


data Tree a = Leaf (Maybe a) | Node (Tree a) a (Tree a) deriving (Eq, Show, Foldable, Functor)

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse f (Leaf Nothing) = pure $ Leaf Nothing
    -- traverse f (Leaf (Just x)) = Leaf <$> (Just <$> f x)
    traverse f (Leaf (Just x)) = Leaf . Just <$> f x
    traverse f (Node t1 x t2) = Node <$> traverse f t1 <*> f x <*> traverse f t2


data Gofri f a
    = MkGofri (f a) (f (Gofri f a))
    deriving (Foldable, Functor)

deriving instance (Eq a, Eq1 f) => Eq (Gofri f a)
deriving instance (Show a, Show1 f) => Show (Gofri f a)

instance Traversable (Traversable f => Gofri f) where
