{-# LANGUAGE LambdaCase #-}

module Hf5 where


import Control.Monad.State
import Control.Monad.Except
import Data.List

-- Definiáljuk az állapotok típusát
data MachineState = Closed | Open | Locked
  deriving (Eq, Show)

-- Definiáljuk a fények típusát
data LightColour = Red | Yellow | Green
  deriving (Eq, Show)

{-

                 ---------    Jegy / Zöld    ----------
             /---|       |------------------>|        |
 Tol / Piros |   | Zárva |    Tol / Zöld     | Nyitva |
             \-->|       |<------------------|        |
                 ---------                   ----------
                   |  ^                          | 
                   |  |       ----------         | Tilt / Piros
                   |  |       |        |         |
      Tilt / Piros |  \-------| Tiltva |<--------/
                   |  Nyit /  |        |
                   |  Zöld    ----------
                   |            ^  | ^
                   |            |  | | Tilt / Piros
                   |            |  | |
                   \------------/  \-/

-}

push, ticket, lock, open :: State MachineState LightColour
push = state $ \case
    Closed -> (Red, Closed)
    Open -> (Green, Closed)
    s -> (Yellow, s)

ticket = state $ \case
    Closed -> (Green, Open)
    s -> (Yellow, s)

lock = state $ \case
    s -> (Red, Locked)

open =  state $ \case
    Locked -> (Green, Closed)
    s -> (Yellow, s)


pistike :: State MachineState [LightColour]
pistike = 
    push >>= \l1 ->
    push >>= \l2 ->
    ticket >>= \l3 ->
    push >>= \l4 ->
    open >>= \l5 ->
    ticket >>= \l6 ->
    push >>= \l7 ->
    return [l1,l2,l3,l4,l5,l6,l7]


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

labelTree :: Num b => Tree a -> State b (Tree (b, a))
labelTree (Leaf x) = 
    get >>= \i ->
    put (i + 1) >>
    return (Leaf (i, x))
labelTree (Node t1 x t2) =
    get >>= \i -> 
    put (i + 1) >>
    labelTree t1 >>= \lt1 ->
    labelTree t2 >>= \lt2 ->
    return (Node lt1 (i, x) lt2)

data SequenceError = LoopDetected Int Int
    deriving (Show, Eq)



nonLoopingSequence :: Eq a => (a -> a) -> a -> Int -> Except SequenceError [a]
nonLoopingSequence f x 0 = return []
nonLoopingSequence f x n = 
    helper f x n >>= \res ->
    return (reverse res) where
        helper :: Eq a => (a -> a) -> a -> Int -> Except SequenceError [a]
        helper f x 1 = return [x]
        helper f x n = 
            helper f x (n-1) >>= \ls@(y:ys) ->
            let ei = elemIndex y ys in
            case ei of
                Nothing -> return (f y : ls)
                (Just i) -> throwError (LoopDetected (length ls - 1) (i + 1))

-- nonLoopingSequence :: Eq a => (a -> a) -> a -> Int -> Except SequenceError [a]
-- nonLoopingSequence f x n = 
--     helper f x n >>= \res ->
--     return (reverse $ fst res) where
--         helper :: Eq a => (a -> a) -> a -> Int -> Except SequenceError ([a], a)
--         helper f x 0 = return ([], x)
--         helper f x n = 
--             helper f x (n-1) >>= \(ls, y) ->
--             let fi = elemIndex y ls in
--             case fi of
--                 Nothing -> return (y : ls, f y)
--                 (Just i) -> throwError (LoopDetected (length ls) (i + 1))


-- nonLoopingSequence f x n = 
    -- let (res, err) = helper f x n where
    --     helper :: Eq a => (a -> a) -> a -> Int -> ([a], Maybe SequenceError)
    --     helper f x 0 = ([], Nothing)
    --     helper f x n = 
    --         let 
    --         (ls, err) = helper f (f x) (n-1) 
    --         fi = findIndex (== x) ls
    --         in case fi of
    --             Nothing -> case err of 
    --                 Nothing -> (x : ls, Nothing)
    --                 je@(Just e) -> (x : ls, je)
    --             (Just i) ->
    --                 (x : ls, Just (LoopDetected ((length ls) - 1) (i + 1)))
    -- in case err of 
    --     Nothing -> return res
    --     (Just e) -> throwError e 
            


            -- get >>= \ls ->
            -- let fi = findIndex (== x) ls in
            -- case fi of
            --     Nothing ->
            --         put (x:ls) >>
            --         helper f (f x) (n-1) >>= \res ->
            --         return res
            --     (Just i) ->
            --         return (LoopDetected ((length ls) - 1) (i + 1))




-- try to do this with a state

-- don't do this with state do this with a simple reverse helper

-- nonLoopingSequence f x 0 = return []
    -- let y = f x in 
    -- nonLoopingSequence f y (n-1) >>= \case
    --     LoopDetected i j 

-- nonLoopingSequence f x n = 
--     helper f x n >>= \res ->
--     return (reverse res) where
--         helper :: Eq a => (a -> a) -> a -> Int -> Except SequenceError [a]
--         helper f x 0 = 
--             get >>= \ls ->
--             return (Right $ reverse ls)
--         helper f x n = 
--             get >>= \ls ->
--             let fi = findIndex (== x) ls in
--             case fi of
--                 Nothing ->
--                     put (x:ls) >>
--                     helper f (f x) (n-1) >>= \res ->
--                     return res
--                 (Just i) ->
--                     return (LoopDetected ((length ls) - 1) (i + 1))