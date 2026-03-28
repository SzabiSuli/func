module Hf7 where

import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List

type MW = MonadWriter [String]
type MS = MonadState [(String, Int)]
type MR = MonadReader Int

runScheduler schedulerMonad initialState initialCoreCount = runWriter $ runStateT (runReaderT schedulerMonad initialCoreCount) initialState

schedule task time = modify (++ [(task, time)])

tick :: (MW m, MS m, MR m) => m ()
tick = do
    n <- ask
    s <- get
    let (xs, ys) = splitAt n s
    let xs' = map (\(a, b) -> (a, b-1)) xs
    let (finished, running) = partition ((<1) . snd) xs'
    put (running ++ ys)
    tell (map fst finished)

reschedule :: (MW m, MS m) => m ()
reschedule = do
    tasks <- get
    let triples = zipWith (\i task -> (i, task, sum (map snd $ drop (i+1) tasks))) [0..] tasks
    let found = find (\(i, (ts, tt), sm) -> tt > sm) triples
    case found of 
        Nothing -> return () -- can only happen if all tasks are at 0 time left
        Just (0, _, _) -> return () -- don't do any changes
        Just (i, x, _) -> do
            let (xs, ys) = splitAt i tasks
            let withoutx = xs ++ drop 1 ys
            put (x : withoutx)
            tell ["Rescheduled task " ++ fst x ++ " from " ++ show i] 
