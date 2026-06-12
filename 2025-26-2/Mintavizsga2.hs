{-# LANGUAGE LambdaCase #-}

module Mintavizsga2 where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable hiding ( asum )
import Data.Functor
import Data.Char
import Data.List
import Data.Monoid
import Debug.Trace
import GHC.Stack

data BiList a b = BiNil | BiCons a b (BiList a b) | ACons a (BiList a b) | BCons b (BiList a b) deriving (Eq, Show)

instance Functor (BiList f) where
    fmap :: (a -> b) -> BiList f a -> BiList f b
    fmap f BiNil = BiNil
    fmap f (BiCons x y l) = BiCons x (f y) $ fmap f l
    fmap f (ACons x l) = ACons x $ fmap f l
    fmap f (BCons y l) = BCons (f y) $ fmap f l

instance Foldable (BiList f) where
    foldr :: (a -> b -> b) -> b -> BiList f a -> b
    foldr f b BiNil = b
    foldr f b (BiCons x y l) = f y $ foldr f b l
    foldr f b (ACons x l) = foldr f b l
    foldr f b (BCons y l) = f y $ foldr f b l

instance Traversable (BiList fx) where
    traverse :: Applicative f => (a -> f b) -> BiList fx a -> f (BiList fx b)
    traverse f BiNil = pure BiNil
    traverse f (BiCons x y l) = BiCons x <$> f y <*> traverse f l
    traverse f (ACons x l) = ACons x <$> traverse f l
    traverse f (BCons y l) = BCons <$> f y <*> traverse f l

b1 :: BiList Double Int
b1 = BiCons 1.0 1 $ ACons 2.0 $ BCons 2 BiNil

b2 :: BiList Bool Char
b2 = BCons 'h' $ BCons 'e' $ BCons 'l' $ BCons 'l' $ BCons 'o' $ BCons ' ' $ BCons 'w' $ BCons 'o' $ BCons 'r' $ BCons 'l' $ BCons 'd' BiNil

b3 :: BiList Bool Bool
b3 = BiCons False True $ b3

b4 :: BiList Int Bool
b4 = ACons 4 BiNil



mapL :: (a -> b) -> BiList a c -> BiList b c
mapL f BiNil = BiNil
mapL f (BiCons x y l) = BiCons (f x) y $ mapL f l
mapL f (ACons x l) = ACons (f x) $ mapL f l
mapL f (BCons y l) = BCons y $ mapL f l

foldL :: Monoid m => (a -> m) -> BiList a b -> m
foldL f BiNil = mempty
foldL f (BiCons x y l) = f x <> foldL f l
foldL f (ACons x l) = f x <> foldL f l
foldL f (BCons y l) = foldL f l

separate :: BiList a b -> ([a], [b])
separate BiNil = ([], [])
separate (BiCons x y l) = 
    let (as , bs) = separate l 
    in (x : as, y : bs)
separate (ACons x l) =
    let (as , bs) = separate l 
    in (x : as, bs)
separate (BCons y l) =
    let (as , bs) = separate l 
    in (as, y : bs)


printLR :: (Show a, Show b) => BiList a b -> IO ()
printLR l = let (as, bs) = separate l
    in do
        foldlM (\x y -> putStr $ (show y) ++ " ") () as 
        putStrLn ""
        foldlM (\x y -> putStr $ (show y) ++ " ") () bs 
        putStrLn ""