module Hf3 where
import Data.Functor.Classes -- Ezt a sort rakd a fájl legelejére



data Tree a = Leaf (Maybe a) | Node (Tree a) a (Tree a) deriving (Eq, Show)
instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f (Leaf Nothing) = mempty
    foldMap f (Leaf (Just a)) = f a
    foldMap f (Node x a y) = foldMap f x <> f a <> foldMap f y



data Gofri f a
    = MkGofri (f a) (f (Gofri f a))

deriving instance (Eq a, Eq1 f) => Eq (Gofri f a)
deriving instance (Show a, Show1 f) => Show (Gofri f a)

instance Foldable Gofri f where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
