{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy01_s where

{-

Tematika:
- Githubon olvasható a hosszab verzió
- Követelmény:
  - Heti házi feladat, mindegyikre 2 hét van, kitöltésük kötelező
  - Félév folyamán 3 db nagybeadandó (3 * 4 pont), nem kötelezőek. 8 pont megszerzése esetén vizsgán +1 jegy ha megvan a kettes
  - Vizsgaidőszakban vizsga az egész féléves gyakorlati tananyagból
  - Előadás: Kedd 10:00
  - Gyakorlatról max 3-szor lehet hiányozni

- A tárgyon tetszőleges IDE és szoftver használható (VSCode, Emacs, Neovim stb) beleértve a Haskell Language Servert
- Vizsgán tetszőleges segédezköz használható emberi segítségen és AI-on kívül

- Órai file-ok: https://github.com/Akaposi/ELTE-func-lang/tree/master/2025-26-2/gyakX
- GyXX_pre.hs = Óra előtti fájl
- GyXX.hs     = Óra utáni fájl

- A tárgy a Funkcionális Programozás (IP-18FUNPEG) tárgyra épül
- Aki el van maradva: lambda.inf.elte.hu

GHCi emlékeztető:
- :l <fájl>     - betölti a fájlt a GHCi-be
- :r            - újratölti a betöltött fájlokat
- :bro <modul>  - browse rövidítése, kiírja egy modul tartalmát
- :t <kif>      - megmondja egy kifejezés típusát
- :i <azon>     - kiírja egy fv/típus/stb információját (kötési erősség, hol van definiálva stb)
- :set <flag>   - bekapcsol egy flag-et (pl -Wincomplete-patterns)
- :q            - kilépés

Pragmák:
{-# <PRAGMA> <OPCIÓK> #-}
- Ez mindig a fájl tetejére megy
- Fontosabb pragmák:
  - OPTIONS_GHC: bekapcsol GHC flageket, pl -Wincomplete-patterns ami warningot ad ha egy mintaillesztés nem totális
  - LANGUAGE: Nyelvi kiegészítők bekapcsolása, pl InstanceSigs ami engedi az instance-ok függvényeinek az explicit típusozását

-}

-- Mai téma: Ismétlés (függvények, mintaillesztés, algebrai adattípusok, típusosztályok)
xor :: Bool -> Bool -> Bool
xor x y = x /= y

-- Több megoldás is lehet (mintaillesztés, beépített függvények)
-- Új "case" kifejezés
{-
-}
xor' x y = case x of
  True -> not y
  False -> y

-- Let/Where kifejezések: lokális definíciók:
twelve :: Int
twelve = x + x
  where
    x = 6

twelve' :: Int
twelve' = let x = 6 in x + x

-- Polimorfizmus: A függvény tetszőleges típusokra működik
id' :: a -> a
id' x = x

-- lehet több típusváltozó is
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (x, (y, (z, w))) = (y, z)

-- Segítség: Hole technológia!
-- Haskellben ha az egyenlőség jobb oldalára _-t írunk, a fordító megmondja milyen típusú kifejezés kell oda

-- Minden függvényre van több megoldás (beépített fügvénnyel pl)

f2 :: (a -> b) -> a -> b
f2 = id

f3 :: (b -> c) -> (a -> b) -> a -> c
-- f3 f g x = f (g x) 
-- f3 f g x = f $ g x 
-- f3 f g x = (f . g) x 
f3 f g = f . g 


f4 :: (a -> b -> c) -> b -> a -> c
f4 f x y = f y x -- flip

-- Segédfüggvények:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

f5 :: ((a, b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5 f x y = f (x,y)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 f (x,y) = f x y 

-- Ha az eredménybe függvényt kell megadni használj lambdákat!
-- pl.: \x -> x

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
-- f7 f = (\ x -> fst (f x), \ x -> snd (f x))
f7 f = (fst . f, snd . f)

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f, g) x = (f x, g x)  

-- ADT-k emlékeztető:
-- Either adattípus. Két konstruktora van, Left és Right, ami vagy a-t vagy b-t tárol:
{-
:i Either
data Either a b = Left a | Right b
-}

f9 :: Either a b -> Either b a
f9 (Left x) = Right x
f9 (Right x) = Left x

f10 :: (Either a b -> c) -> (a -> c, b -> c)
f10 f = (\ x -> f (Left x), \ x -> f (Right x))

f11 :: (a -> c, b -> c) -> (Either a b -> c)
f11 (f, _) (Left x) = f x 
f11 (_, g) (Right x) = g x

-- Bónusz

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 (Left (x, y)) = (x, Left y)
f12 (Right (x, y)) = (x, Right y)

f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 (x, Left y) = Left (x,y)
f13 (x, Right y) = Right (x,y)

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 f g = f y y where
  y = g (\ x -> f x x)

-- Általánosított Leöb függvény


-- Listák emlékeztető
-- Hogyan is van a lista definiálva?

-- Definiáljuk a map, filter függvényeket listagenerátorral, rekurzióval és hajtogatással

map' :: (a -> b) -> [a] -> [b]
map' f [] = [] 
map' f (x:xs) = f x : map' f xs 

filter' :: (a -> Bool) -> [a] -> [a]
-- filter' p [] = [] 
-- filter' p (x:xs) = case p x of
--   True -> x : filter' p xs
--   False -> filter' p xs 
filter' p xs = foldr (\x filteredXs -> if p x then x : filteredXs else filteredXs) [] xs

-- Definiáljunk egyéb hasznos lista függvényeket, amelyek részei a standard librarynek.
-- !! Vizsgán érdemes nem újrainventálni a teljes Haskell stdlib-et !!

take', drop' :: Int -> [a] -> [a]

take' = undefined
drop' = undefined

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' = undefined

takeWhile', dropWhile' :: (a -> Bool) -> [a] -> [a]

takeWhile' = undefined
dropWhile' = undefined

span', partition' :: (a -> Bool) -> [a] -> ([a], [a])

span' = undefined
partition' = undefined

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = undefined

cycle' :: [a] -> [a]
cycle' = undefined

iterate' :: a -> (a -> a) -> [a]
iterate' = undefined

repeat' :: a -> [a]
repeat' = undefined

replicate' :: Int -> a -> [a]
replicate' = undefined

nub' :: Eq a => [a] -> [a]
nub' = undefined


-- practice

f15 :: (a -> b -> c) -> (a -> b) -> a -> c
f15 f g x = f x (g x)

f16 :: ((a -> b) -> c) -> (c -> a) -> b
f16 g h = _

f17 :: ((a -> b) -> b) -> ((b -> a) -> a) -> b
f17 g h = g $ \ a -> _

f18 :: ((a -> b) -> a) -> a
f18 g = g (\ x -> _b)

