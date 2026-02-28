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

instance Foldable f => Foldable (Gofri f) where
    foldMap :: Monoid m => (a -> m) -> Gofri f a -> m
    foldMap f (MkGofri fa x) = foldMap f fa <> foldMap (foldMap f) x


data CrazyType3 a b
    = CrazyCon1 a b a 
    | CrazyCon2 (CrazyType3 a b) [b] [a]
    | CrazyCon3 (CrazyType3 Int b) (CrazyType3 a a) [[b]]
    deriving (Eq, Show)

instance Foldable (CrazyType3 a) where
    foldMap :: Monoid m => (b -> m) -> CrazyType3 a b -> m
    foldMap f (CrazyCon1 x y z) = f y
    foldMap f (CrazyCon2 x lb la) = foldMap f x <> foldMap f lb
    foldMap f (CrazyCon3 ctib cta llb) = foldMap f ctib <> foldMap (foldMap f) llb



foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty
-- foldMap' f x = foldr (\a m -> f a <> m) mempty x

foldr' :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr' f b x = destructEndo (foldMap (Endo . f) x) b
-- foldr' f b x = destructEndo (foldMap (\a -> Endo (f a)) x) b

-- we can pass \a -> f a b
-- if it knows mempty = b
-- and x
-- we need to make a monoid from b


-- the solution is, 
-- we can't provide a default for a generic b parameter,
-- because b could be empty
-- but we can make a type that 
-- IF it recieves an element, it could carry that through with the <> operator

newtype Endo b = Endo (b -> b)

instance Semigroup (Endo b) where
    (<>) :: Endo b -> Endo b -> Endo b
    Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo b) where
    mempty = Endo id

destructEndo :: Endo b -> (b -> b)
destructEndo (Endo f) = f


foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldl (\m a -> m <> f a) mempty

foldl' :: (Foldable t) => (b -> a -> b) -> b -> t a -> b
-- foldl' f b x = destructEndo (foldMap (\a -> Endo (\y -> f y a)) x) b
-- foldl' f b x = destructEndo (foldMap (\a -> Endo (flip f a)) x) b
foldl' f b x = destructEndo (foldMap (Endo . flip f) x) b
