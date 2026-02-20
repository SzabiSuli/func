{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{- HLINT ignore "Use newtype instead of data" -}

module Gy02_s where

import Prelude hiding (Either (..), Maybe (..))
import Data.Kind (Type)
import Unsafe.Coerce
import Distribution.Compat.Lens (_1)


-- Vegyük az alábbi adattípusokat
data Single a = Single a deriving (Eq, Show)
data Tuple a = Tuple a a deriving (Eq, Show)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)
data Maybe a = Just a | Nothing deriving (Eq, Show)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show)
data Either e a = Left e | Right a deriving (Eq, Show)
data BiTuple e a = BiTuple e a deriving (Eq, Show)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show)

-- Próbáljunk meg olyan függvényeket írni, ami a fent említett típusoknak a típusparaméterét megváltoztatja
-- Pl.: Single a -> Single b vagy List a -> List b
-- Mivel a fenti típusok mind valamilyen szinten tárolnak magukban 'a' típusú elemet ezért szükséges lesz egy (a -> b) függvényre

mapSingle :: (a -> b) -> Single a -> Single b
mapSingle f (Single a) = Single $ f a

mapTuple :: (a -> b) -> Tuple a -> Tuple b
mapTuple f (Tuple x y) = Tuple (f x) $ f y 

mapQuintuple :: (a -> b) -> Quintuple a -> Quintuple b
mapQuintuple = undefined

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just $ f a

mapList :: (a -> b) -> List a -> List b
mapList f Nil = Nil
mapList f (Cons x xs) = Cons (f x) $ mapList f xs


-- Emeljük ki a Single, Tuple stb-t a típusból (ezt hívják magasabbrendű polimorfizmusnak, mert a polimorfizmust típusfüggvényekre alkalmazzuk):
{-

        mapSingle    :: (a -> b) -> Single    a -> Single    b
        mapTuple     :: (a -> b) -> Tuple     a -> Tuple     b
        mapQuintuple :: (a -> b) -> Quintuple a -> Quintuple b

        map          :: (a -> b) ->     f     a ->     f     b

-}

-- Ezt fogjuk Functornak hívni
{-
:i Functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
        -- Defined in ‘GHC.Base’
-}

-- A Functornak szabálya konyhanyelven: megtartja az adat struktúráját
-- Tehát a konstruktorok sorrendjét, helyét és számát nem változtatja.

instance Functor Single where
  fmap :: (a -> b) -> Single a -> Single b
  fmap = mapSingle

instance Functor Tuple where
  fmap :: (a -> b) -> Tuple a -> Tuple b
  fmap = mapTuple

instance Functor Quintuple where
  fmap :: (a -> b) -> Quintuple a -> Quintuple b
  fmap = mapQuintuple

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = mapMaybe

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap = mapList

-- Írjuk meg a többi típusra is a Functor instance-ot!

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (Last a) = Last $ f a 
  fmap f (NECons a xs) = NECons (f a) $ fmap f xs 

instance Functor NonEmpty2 where
  fmap :: (a -> b) -> NonEmpty2 a -> NonEmpty2 b
  fmap f (NECons2 a xs) = NECons2 (f a) $ fmap f xs 

-- Ugye a Functor egy Type -> Type kindú kifejezést vár, viszont pl az Either egy Type -> Type -> Type kindú valami, ezért le kell fixálni az első paramétert


instance Functor (Either fixed) where
  fmap :: (a -> b) -> Either fixed a -> Either fixed b
  fmap f (Right x) = Right $ f x
  fmap f (Left x) = Left x
  -- fmap f x = unsafeCoerce (x :: Either fixed a ) :: Either fixed b

instance Functor (BiTuple fixed) where
  fmap :: (a -> b) -> BiTuple fixed a -> BiTuple fixed b
  fmap = undefined

instance Functor (TriEither fixed1 fixed2) where
  fmap :: (a -> b) -> TriEither fixed1 fixed2 a -> TriEither fixed1 fixed2 b
  fmap = undefined

instance Functor (BiList fixed) where
  fmap :: (a -> b) -> BiList fixed a -> BiList fixed b
  fmap = undefined

-- "nagyon" magasabbrendú polimorfizmus. Ha egy Type -> Type kindú valamit és egy típust adunk meg, csak akkor lesz teljes

-- Speciális Kind annotáció, hogy minden kind-ját megadjuk, ritkán szükséges
-- Én csak az egyszerűség kedvéért kommentbe majd odaírom
type    Lift :: (Type -> Type) -> Type -> Type
newtype Lift f a = Lift (f a) deriving (Eq, Show)

-- data BiTuple e a = BiTuple e a deriving (Eq, Show)
-- data Lift    f a = Lift   (f a) deriving (Eq, Show)
-- Van különbség

-- Példa:
listOfInts :: Lift List Int
listOfInts = Lift (Cons 1 (Cons 2 Nil))

listOfInts' :: List Int
listOfInts' = Cons 1 (Cons 2 Nil)

maybeABool :: Lift Maybe Bool
maybeABool = Lift Nothing -- pont nincs bool :(

-- Le kell az első paramétert fixálnunk, hogy tudjunk rá Functor-t írni
-- Viszont a fix típusra kell Functor kikötés, hogy az a-t kicserélhessük benne
instance (Functor f) => Functor (Lift f) where
  fmap :: (Functor f) => (a -> b) -> Lift f a -> Lift f b
  fmap f (Lift x) = Lift $ fmap f x

-- f az vmi funktor
-- g : a -> b
-- fa : Functor f => f a

-- Pár hasonló típus
data Sum f g a = SumLeft (f a) | SumRight (g a) deriving (Eq, Show)
data Product f g a = Product (f a) (g a) deriving (Eq, Show)
data Compose f g a = Compose (f (g a)) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Sum f g a -> Sum f g b
  fmap f (SumLeft x) = SumLeft $ fmap f x
  fmap f (SumRight x) = SumRight $ fmap f x

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Product f g a -> Product f g b
  fmap f (Product x y) = Product (fmap f x) $ fmap f y

-- Nehéz

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose x) = Compose $ fmap (fmap f) x

-- A függvény funktor?
data Fun a b = Fun (a -> b)

instance Functor (Fun q) where
  -- fmap :: (a -> b) -> (q -> a) -> (q -> b)
  -- Hint: mi a (.) típusa?
  fmap :: (a -> b) -> Fun q a -> Fun q b
  -- fmap f (Fun g) = Fun (\a -> f (g a))
  fmap f (Fun g) = Fun $ f . g

-- Egyéb érdekesség:
data UselessF f a = Mk1 (f Int) a
--                       ^ f nincs olyan pozícióban, hogy fmap-olni kéne rajta, tehát a Functor f megkötés felesleges



-- Gyakorlás:

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)
data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving (Eq, Show)
data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)

data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)
instance Functor CrazyType where
  fmap :: (a -> b) -> CrazyType a -> CrazyType b
  fmap f (C1 x y) = C1 (f x) $ f y
  fmap f (C2 x i) = C2 (f x) i
  fmap f (C3 x) = C3 $ fmap f x

data Either3 a b c = Left3 a | Middle3 b | Right3 c deriving (Eq, Show) -- just map c
data Triplet a b c = Triplet a b c deriving (Eq, Show) -- same here
data SplitTree a b = SplitTree (Tree a) a b (Tree b) deriving (Eq, Show) -- same here

data TriCompose f g h a = TriCompose (f (g (h a))) deriving (Eq, Show)
instance (Functor f, Functor g, Functor h) => Functor (TriCompose f g h) where
  fmap :: (a -> b) -> TriCompose f g h a -> TriCompose f g h b
  fmap f (TriCompose x) = TriCompose $ fmap (fmap (fmap f)) x

data Free f a = Pure a | Free (f (Free f a))
instance (Functor f) => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure a) = Pure $ f a
  fmap f (Free x) = Free (fmap (fmap f) x)

type Fix :: (Type -> Type) -> Type -> Type
data Fix f a = Fix (f (Fix f a))
instance (Functor f) => Functor (Fix f) where
  fmap :: (a -> b) -> Fix f a -> Fix f b
  fmap f (Fix x) = Fix $ fmap (fmap f) x

data Join a b = Join (a -> a -> b)
instance Functor (Join a) where
  fmap :: (b -> c) -> Join a b -> Join a c
  fmap f (Join g) = Join $ \x y-> f (g x y)

data CrazyType2 a b = SingleA a | SingleB b | Translate (a -> b)
instance Functor (CrazyType2 a) where
  fmap :: (b -> c) -> CrazyType2 a b -> CrazyType2 a c
  fmap f (SingleA a) = SingleA a
  fmap f (SingleB b) = SingleB $ f b
  fmap f (Translate g) = Translate $ f . g

--- Írjunk rájuk Functor instance-ot!

-- Dont mind this

deriving instance (Eq a, forall q. (Eq q) => Eq (f q)) => Eq (Free f a)

deriving instance (Show a, forall q. (Show q) => Show (f q)) => Show (Free f a)

deriving instance (Eq a, forall q. (Eq q) => Eq (f q)) => Eq (Fix f a)

deriving instance (Show a, forall q. (Show q) => Show (f q)) => Show (Fix f a)

deriving instance (Show a, Show (f Int)) => Show (UselessF f a)

deriving instance (Eq a, Eq (f Int)) => Eq (UselessF f a)



{-
Functor algoritmus (ahol csak Functor F-et engedünk meg)
-- 1. illesszük le az összes konstruktort
-- 2. Konstruáljunk azzal a konstruktorral
-- 3. minden változóra ha
  -- az x : a akkor rakjunk helyette (f x)-t
  -- ha y : c ahol c nem tartalmaz a típust akkor rakjunk oda y-t
  -- ha z : (f a) van ahol f Functor akkor hívjunk fmap f z -t
    -- ha z : (f (f a)) akkor fmap (fmap f) z-t stb... 


Functor algoritmus (ahol engedüng f Functort és q ContraFunctort is)
...
-}

-- Gemini által generált feladatok ContraFunctorra

-- =========================================================================
-- ContraFunctor és kevert feladatok
-- =========================================================================

-- Mivel a standard könyvtárban ez Contravariant néven szerepel, 
-- definiáljuk a te elnevezéseddel a gyakorláshoz:
class ContraFunctor f where
  contramap :: (b -> a) -> f a -> f b

-- 1. Alapvető ContraFunctor példák
-- Próbáld meg kitalálni, hogyan kell a (b -> a) függvényt felhasználni!

data Predicate a = Predicate (a -> Bool) -- deriving (Eq, Show) 

instance ContraFunctor Predicate where
  contramap :: (b -> a) -> Predicate a -> Predicate b
  contramap f (Predicate p) = Predicate $ p . f

data Op r a = Op (a -> r)

instance ContraFunctor (Op r) where
  contramap :: (b -> a) -> Op r a -> Op r b
  contramap f (Op g) = Op $ g . f

data Comparator a = Comparator (a -> a -> Ordering)

instance ContraFunctor Comparator where
  contramap :: (b -> a) -> Comparator a -> Comparator b
  contramap f (Comparator g) = Comparator $ \x y -> g (f x) (f y)


-- 2. Kompozíciók (Functor és ContraFunctor keverése)
-- Itt az a feladat, hogy rájöjj: vajon a kompozíció eredménye Functor vagy ContraFunctor lesz?

data ComposeFC f q a = ComposeFC (f (q a)) deriving (Eq, Show)

instance (Functor f, ContraFunctor q) => ContraFunctor (ComposeFC f q) where
  contramap :: (Functor f, ContraFunctor q) => (b -> a) -> ComposeFC f q a -> ComposeFC f q b
  contramap f (ComposeFC x) = ComposeFC $ fmap (contramap f) x 


data ComposeCF q f a = ComposeCF (q (f a)) deriving (Eq, Show)

instance (ContraFunctor q, Functor f) => ContraFunctor (ComposeCF q f) where
  contramap :: (ContraFunctor q, Functor f) => (b -> a) -> ComposeCF q f a -> ComposeCF q f b
  contramap = undefined


-- Figyeld meg az alábbi típust és az instance-ot! Mi történik, ha két ContraFunctor-t ágyazunk egymásba?
data ComposeCC q1 q2 a = ComposeCC (q1 (q2 a)) deriving (Eq, Show)

instance (ContraFunctor q1, ContraFunctor q2) => Functor (ComposeCC q1 q2) where
  fmap :: (ContraFunctor q1, ContraFunctor q2) => (a -> b) -> ComposeCC q1 q2 a -> ComposeCC q1 q2 b
  fmap f (ComposeCC x) = ComposeCC $ contramap (contramap f) x


-- 3. Bonyolultabb, kevert struktúrák
-- Ebben a szekcióban a típusparaméterek és a függvények irányai extra csavarokat rejtenek.

data Tricky q a = Tricky (q (a -> Int))

instance ContraFunctor q => Functor (Tricky q) where
  fmap :: ContraFunctor q => (a -> b) -> Tricky q a -> Tricky q b
  -- fmap f (Tricky x) = Tricky $ contramap (\g -> g . f) x
  fmap f (Tricky x) = Tricky $ contramap (. f) x


data Tricky2 f a = Tricky2 (f a -> Int)

instance Functor f => ContraFunctor (Tricky2 f) where
  contramap :: Functor f => (b -> a) -> Tricky2 f a -> Tricky2 f b
  contramap f (Tricky2 x) = Tricky2 $ \fb -> x $ fmap f fb


data MixedStructure f q a = MixedStructure (f (q (q a))) (q (a -> String))

instance (Functor f, ContraFunctor q) => Functor (MixedStructure f q) where
  fmap :: (Functor f, ContraFunctor q) => (a -> b) -> MixedStructure f q a -> MixedStructure f q b
  fmap f (MixedStructure x y) = MixedStructure u v where
    u = fmap (contramap (contramap f)) x
    v = contramap (. f) y


data UltimateMix f q a = UMix (q (f a)) (a -> f Int) (f (q (Bool -> a))) 

instance (Functor f, ContraFunctor q) => ContraFunctor (UltimateMix f q) where
  contramap :: (Functor f, ContraFunctor q) => (b -> a) -> UltimateMix f q a -> UltimateMix f q b
  contramap f (UMix x y z) = UMix u v w where
    u = contramap (fmap f) x
    v = y . f
    w = fmap (contramap (f .)) z
