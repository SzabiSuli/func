module Hf1 where

mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' f [] = []
mapMaybe' f (x:xs) = case f x of
    Nothing -> mapMaybe' f xs
    Just y -> y : mapMaybe' f xs

intersperse' :: a -> [a] -> [a]
intersperse' s [] = []
intersperse' s (x:xs) = x : intersperse'' xs where
    intersperse'' [] = []
    intersperse'' (x:xs) = s : x : intersperse'' xs

pascalTriangle :: Num a => [[a]]
pascalTriangle = [1] : pascalHelper [1] where
    -- pascalHelper :: [a1] -> [[a1]]
    pascalHelper l = nextl : pascalHelper nextl where
        nextl = map (\ (x,y) -> x + y) $ zip (0 : l) (l ++ [0])