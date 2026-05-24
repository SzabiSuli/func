{-# LANGUAGE LambdaCase #-}

module Mintavizsga1 where

import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad
import Data.List
import Data.Foldable

data LengthIndexedList i a = Nil | Cons i a (LengthIndexedList i a) deriving (Eq, Show)


instance Functor (LengthIndexedList i) where
    fmap :: (a -> b) -> LengthIndexedList i a -> LengthIndexedList i b
    fmap f Nil = Nil
    fmap f (Cons i x l) = Cons i (f x) (fmap f l)


instance Foldable (LengthIndexedList i) where
    foldr :: (a -> b -> b) -> b -> LengthIndexedList i a -> b
    foldr f b Nil = b
    foldr f b (Cons i x l) = f x $ foldr f b l

instance Traversable (LengthIndexedList i) where
    traverse :: Applicative f => (a -> f b) -> LengthIndexedList i a -> f (LengthIndexedList i b)
    traverse f Nil = pure Nil
    traverse f (Cons i x l) = Cons i <$> f x <*> traverse f l

l1 :: LengthIndexedList Integer Char
l1 = Cons 11 'h' $ Cons 10 'e' $ Cons 9 'l' $ Cons 8 'l' $ Cons 7 'o' $ Cons 6 ' ' $ Cons 5 'w' $ Cons 4 'o' $ Cons 3 'r' $ Cons 2 'l' $ Cons 1 'd' $ Nil

l2 :: LengthIndexedList Int Bool
l2 = let l@(Cons i a r) = fmap not l2 in Cons (i + 1) True l

l3 :: LengthIndexedList Integer Int
l3 = Cons 2 2 $ Cons 1 1 Nil


satisfyInvariant :: (Num i, Eq i) => LengthIndexedList i a -> Bool
satisfyInvariant Nil = True
satisfyInvariant ls@(Cons i x l) = satisfyInvariant' i ls where
    satisfyInvariant' :: (Num i, Eq i) => i -> LengthIndexedList i a -> Bool
    satisfyInvariant' 0 Nil = True
    satisfyInvariant' 0 (Cons _ _ _) = False
    satisfyInvariant' exp Nil = False
    satisfyInvariant' exp (Cons i x l) = satisfyInvariant' (exp - 1) l

mkLIL :: (Foldable f, Num i) => f a -> LengthIndexedList i a
mkLIL = foldr folder Nil where
    folder :: Num i => (a -> LengthIndexedList i a -> LengthIndexedList i a)
    folder x Nil = Cons 1 x Nil
    folder x ls@(Cons i y l) = Cons (i + 1) x ls

reverseLIL :: Num i => LengthIndexedList i a -> LengthIndexedList i a
reverseLIL = reverseLIL' 0 Nil where
    reverseLIL' :: Num i => i -> LengthIndexedList i a -> LengthIndexedList i a -> LengthIndexedList i a
    reverseLIL' len acc Nil = acc
    reverseLIL' len acc (Cons i x l) = reverseLIL' (len + 1) (Cons (len + 1) x acc) l



-- Monad part

type Coords = (Int, Int)

type MW = MonadWriter [Coords]
type MS = MonadState (Coords -> Bool)

-- runLawnmover :: (MW m, MS m) => m a -> s -> m ((a, s), w)
runLawnmover lawnmoverMonad initialState = runWriterT $ runStateT lawnmoverMonad initialState

mowAt :: (MW m, MS m) => Coords -> m ()
mowAt coords = do
    lawn <- get
    when (lawn coords) $ do
        tell [coords]
        put $ lawn' lawn coords where
            lawn' lawn mowed c 
                | mowed == c = False
                | otherwise = lawn c 

drawLawn :: (MW m, MS m, MonadIO m) => Coords -> Coords -> m ()
drawLawn (y1, x1) c2@(y2, x2) 
    | x1 > x2 || y1 > y2 = pure ()
    | otherwise = do 
        printRow y1 x1 x2 
        drawLawn (y1 + 1, x1) c2 
        where
            printRow :: (MW m, MS m, MonadIO m) => Int -> Int -> Int -> m ()
            printRow y x1 x2 
                | x1 > x2 = liftIO $ putStrLn "" -- print new line
                | otherwise = do 
                    lawn <- get
                    if lawn (y, x1) 
                        then liftIO $ putStr "* "
                        else liftIO $ putStr "# "
                    printRow y (x1 + 1) x2


