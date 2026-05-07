{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Bead1 where

import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.String
import Data.Maybe
import Debug.Trace

data ProgState = ProgState {
    r1     :: Int,
    r2     :: Int,
    r3     :: Int,
    cmp    :: Ordering,
    memory :: [Int]
} deriving (Eq, Show)

startState :: ProgState
startState = ProgState 0 0 0 EQ (replicate 10 0)

type Label = String  -- címke a programban, ahová ugrani lehet

data Register
  = R1
  | R2
  | R3
  deriving (Eq, Show)

data Destination
  = DstReg Register     -- regiszterbe írunk
  | DstDeref Register   -- memóriába írunk, az adott regiszterben tárolt index helyére
  deriving (Eq, Show)

data Source
  = SrcReg Register     -- regiszterből olvasunk
  | SrcDeref Register   -- memóriából olvasunk, az adott regiszterben tárolt index helyéről
  | SrcLit Int          -- szám literál
  deriving (Eq, Show)

data Instruction
  = Mov Destination Source   -- írjuk a Destination-be a Source értékét
  | Add Destination Source   -- adjuk a Destination-höz a Source értékét
  | Mul Destination Source   -- szorozzuk a Destination-t a Source értékével
  | Sub Destination Source   -- vonjuk ki a Destination-ből a Source értékét
  | Cmp Source Source        -- hasonlítsunk össze két Source értéket `compare`-el, az eredményt
                             -- írjuk a `cmp` regiszterbe

  | Jeq Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `EQ` van
  | Jlt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `LT` van
  | Jgt Label                -- Ugorjunk az adott címkére ha a `cmp` regiszterben `GT` van
  deriving (Eq, Show)

type RawProgram = [Either Label Instruction]

-- Beírunk r1-be 10-et, r2-be 20-at
p1 :: RawProgram
p1 = [
  Left "start",
  Right $ Mov (DstReg R1) (SrcLit 10),
  Left "l1",                            -- tehetünk bárhova címkét, nem muszáj használni a programban
  Right $ Mov (DstReg R2) (SrcLit 20)
  ]

type Program = [(Label, [Instruction])]

toProgram :: RawProgram -> Program
toProgram [] = []
toProgram ((Left l):xs) = (l, allInstructions xs) : toProgram xs
toProgram ((Right _):xs) = toProgram xs


allInstructions :: RawProgram -> [Instruction]
allInstructions [] = []
allInstructions ((Left _):xs) = allInstructions xs
allInstructions ((Right i):xs) = i : allInstructions xs


type M a = State ProgState a

eval :: Program -> [Instruction] -> M ()
eval p [] = return ()
eval p (i : is) = undefined
  -- do
  -- ...
  -- return eval p is


exec :: Program -> [Instruction] -> State ProgState [Instruction]
exec p [] = return [] 
-- exec p ((Mov dst src) : is) = getSrc src >>= putDst dst >> exec p is
exec p ((Mov dst src) : is) = modifyDst dst src (flip const) >> exec p is
exec p ((Add dst src) : is) = modifyDst dst src (+) >> exec p is
exec p ((Mul dst src) : is) = modifyDst dst src (*) >> exec p is
exec p ((Sub dst src) : is) = modifyDst dst src (-) >> exec p is
exec p ((Cmp src1 src2) : is) = undefined
exec p ((Jeq l) : is) = undefined
exec p ((Jlt l) : is) = undefined
exec p ((Jgt l) : is) = undefined

modifyDst :: Destination -> Source -> (Int -> Int -> Int) -> State ProgState ()
modifyDst dst src f = do
  s <- getSrc src
  d <- getSrc $ dstAsSrc dst
  putDst dst (f d s)

dstAsSrc :: Destination -> Source
dstAsSrc (DstReg d) = SrcReg d
dstAsSrc (DstDeref d) = SrcDeref d

getSrc :: Source -> State ProgState Int
getSrc (SrcReg r) = getR r
  -- ps <- get
  -- return $ r1 ps 
-- getSrc (SrcReg r) = get >>= r2
--   -- ps <- get
--   -- return $ r2 ps 
-- getSrc (SrcReg r) = get >>= r3
  -- ps <- get
  -- return $ r3 ps 
getSrc (SrcDeref d) = do
  addr <- getR d
  ps <- get
  return (memory ps !! addr)
getSrc (SrcLit l) = return l


getR :: Register -> State ProgState Int
getR R1 = get >>= \ps -> return (r1 ps)
getR R2 = get >>= \ps -> return (r2 ps)
getR R3 = get >>= \ps -> return (r3 ps)

putDst :: Destination -> Int -> State ProgState ()
putDst (DstReg r) x = do
  ps <- get
  case r of  
    R1 -> put (ps {r1 = x})
    R2 -> put (ps {r2 = x})
    R3 -> put (ps {r3 = x})
putDst (DstDeref r) x = do
  addr <- getR r
  ps <- get
  put (ps {memory = replaceAtIndex addr x (memory ps)})

-- putCmp :: TODO cont here

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i newVal xs = 
  let (before, _:after) = splitAt i xs
  in before ++ [newVal] ++ after

-- futtatunk egy nyers programot a startState-ből kiindulva
runProgram :: RawProgram -> ProgState
runProgram rprog = case toProgram rprog of
  []                  -> startState
  prog@((_, start):_) -> execState (eval prog start) startState