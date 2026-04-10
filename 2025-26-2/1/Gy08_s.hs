{-# LANGUAGE DeriveFoldable, DeriveFunctor, QuantifiedConstraints, StandaloneDeriving #-}
{- HLINT ignore "Use newtype instead of data" -}
module Gy08 where

import Prelude hiding (Maybe(..), Either(..))

-- Definiáljuk egy függvényt, amely egy "mellékhatásos" függvényt végig mappol egy listán
--                                          v az m mellékhatást összegyűjtjük
mapMList :: Monad m => (a -> m b) -> [a] -> m [b]
mapMList f [] = return []
mapMList f (x:xs) = do 
  x' <- f x 
  xs' <- mapMList f xs
  return $ x' : xs'


-- Mivel a Functor (sima mappolás) általánosítható volt, ez a mellékhatásos mappolás is lehet általánosítható
data Single a = Single a deriving (Eq, Show, Functor, Foldable)
data Tuple a = Tuple a a deriving (Eq, Show, Functor, Foldable)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show, Functor, Foldable)
data List a = Nil | Cons a (List a) deriving (Eq, Show, Functor, Foldable)
data Maybe a = Just a | Nothing deriving (Eq, Show, Functor, Foldable)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Functor, Foldable)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show, Functor, Foldable)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show, Functor, Foldable)
data Either e a = Left e | Right a deriving (Eq, Show, Functor, Foldable)
data BiTuple e a = BiTuple e a deriving (Eq, Show, Functor, Foldable)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show, Functor, Foldable)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show, Functor, Foldable)
data Apply f a = MkApply (f a) deriving (Eq, Show, Functor, Foldable)
data Fix f a = MkFix (f (Fix f a)) deriving (Functor, Foldable)
data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show, Functor, Foldable)
data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show, Functor, Foldable)
data Prod f a b = FProd (f a) (f b) deriving (Eq, Show, Functor, Foldable)
data FList f a = FNil | FCons (f a) (f (FList f a)) deriving (Functor, Foldable)

-- Írjuk meg ezt a műveletet pár fenti típusra!
mapMSingle :: Monad m => (a -> m b) -> Single a -> m (Single b)
mapMSingle f (Single a) = do
  x <- f a
  return (Single x)

mapMTuple :: Monad m => (a -> m b) -> Tuple a -> m (Tuple b)
mapMTuple f t = do
  let xs = fmap f t
  undefined

mapMQuintuple :: Monad m => (a -> m b) -> Quintuple a -> m (Quintuple b)
mapMQuintuple = undefined

mapMMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMMaybe _ Nothing = return Nothing
mapMMaybe f (Just x) = do
  x' <- f x
  return (Just x')

-- Ehhez a mellékhatásos mappoláshoz viszont a Monád megkötés sokat enged meg
-- A monád fő művelete a (>>=) :: m a -> (a -> m b) -> m b
-- Ami mellékhatásos műveletek közti függőséged modellez.
-- Viszont mappolásnál az egyes elemek között eredmény alapú függőség nincs
-- Applicative típusosztály: A Functor és a Monád között van
{-
:i Applicative
type Applicative :: (* -> *) -> Constraint
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
        -- Defined in ‘Control.Applicative’
-}
-- A típusosztály koncepciója az fmap művelet általánosítása tetszőleges paraméterű függvényre, pl.:
-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- Ehhez viszont szükség van (egymástól független) mellékhatások kombinációjára
-- liftA műveletek csak liftA3-ig vannak standard libraryben, viszont arbitrary liftA írtható a <*> segítségével
-- pl.:
liftA4 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA4 func fa fb fc = func <$> fa <*> fb <*> fc
-- func :: a -> b -> c -> d
-- func <$> fa :: f (b -> c -> d)
-- func <$> fa <*> fb :: f (c -> d)
-- func <$> fa <*> fb <*> fc :: f d
-- Ezeket az ún app láncot fogjuk használni mapA írásnál is!
-- Írjuk meg a mapM műveletet Applicative segítségével
-- Az algoritmus ugyanaz mint a funktornál csak függvényalkalmazás helyett <*> és független értékek esetén pure

-- ez a traverse
mapA :: Applicative f => (a -> f b) -> List a -> f (List b)
mapA f Nil = pure Nil
mapA f (Cons x xs) = (Cons <$> f x) <*> mapA f xs
-- mapA f (Cons x xs) = Cons <$> f x <*> mapA f xs
-- <$>  fmap csak infix

-- pontosan "a" a paraméter típusa -> f a
-- függ "a"-tól                    -> ? f a
-- nem függ "a"-tól                -> pure a

-- foldr-t meg lehet írni traverse-el


-- Ez a mappolhatósági tulajdonság lesz az úgynevezett Traversable típusosztály
{-
:i Traversable
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
        -- Defined in ‘Data.Traversable’
-}

instance Traversable Single where
  traverse :: Applicative f => (a -> f b) -> Single a -> f (Single b)
  traverse f (Single x) = Single <$> f x

instance Traversable Tuple where
  traverse :: Applicative f => (a -> f b) -> Tuple a -> f (Tuple b)
  traverse f (Tuple x y) = Tuple <$> f x <*> f y

instance Traversable Quintuple where
  traverse :: Applicative f => (a -> f b) -> Quintuple a -> f (Quintuple b)
  traverse f (Quintuple x y z v w) = Quintuple <$> f x <*> f y <*> f z <*> f v <*> f w

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse = mapA

  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance Traversable Maybe where
  traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse f Nothing = pure Nothing
  traverse f (Just x) = Just <$> f x

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (Last a) = Last <$> f a
  traverse f (NECons x xs) = NECons <$> f x <*> traverse f xs

instance Traversable NonEmpty2 where

instance Traversable Tree where

instance Traversable (Either fixed) where

instance Traversable (BiTuple fixed) where

instance Traversable (TriEither fixed1 fixed2) where

instance Traversable (BiList fixed) where

-- Magasabbrendű megkötések
instance Traversable f => Traversable (Apply f) where
  -- traverse :: (Traversable f, Applicative f1) => (a -> f1 b) -> Fix f a -> f1 (Fix f b)
  -- traverse f (MkFix fa) = MkFix <$> _

instance Traversable f => Traversable (Fix f) where
  traverse :: (Traversable f, Applicative f1) => (a -> f1 b) -> Fix f a -> f1 (Fix f b)
  traverse f (MkFix fa) = MkFix <$> _ --(fmap f fa)

instance (Traversable f, Traversable g) => Traversable (Compose f g) where

instance Traversable f => Traversable (Sum f fixed) where

instance Traversable f => Traversable (Prod f fixed) where

instance Traversable f => Traversable (FList f) where

-- Kiegészítő tananyag: Applicative Do

-- Mágia, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)

-- extra feladat
labelTrav :: Traversable t => t a -> t (a , Int)
labelTrav = undefined