module Hf6 where

import Control.Monad.Reader
import Control.Monad.Writer

mapMaybeWithIndexR :: Num i => (i -> a -> Maybe b) -> [a] -> Reader i [b]
mapMaybeWithIndexR f [] = return [] 
mapMaybeWithIndexR f (x : xs) =
    ask >>= \i ->
    case f i x of 
        Nothing -> local (+1) $ mapMaybeWithIndexR f xs
        Just y -> local (+1) $ mapMaybeWithIndexR f xs >>= \ys -> 
            return $ y : ys

mapMaybeWithIndex :: Num i => (i -> a -> Maybe b) -> [a] -> [b]
mapMaybeWithIndex f l = runReader (mapMaybeWithIndexR f l) 0


data Tree a = RoseTree a [Tree a] deriving (Eq, Show, Functor, Foldable)

subtreesW :: Tree a -> Writer [Tree a] ()
subtreesW t@(RoseTree _ xs) = 
    tell [t] >>
    foldr (\x -> (>>) (subtreesW x)) (return ()) xs

subtrees :: Tree a -> [Tree a]
subtrees t = execWriter (subtreesW t)
