-- 10:15-ös kezdés
-- Funkcionális nyelvek = Advanced Haskell

-- előadó: szumi@inf.elte.hu (Szumi Xie)
-- 10:15-11:00 11:05-11:50

{- 
Haskell:
statically-typed
lazy
pure
functional programming language

- programming language: 
    - formal language: based on formal rules
-functional programming language:
    - furst-class functions: functions can be used as values
    - what are functions?
        - in math: given an input, there is an output
            - two equal inputs give equal outputs

- pure language: no side effects
    but you can do
    x = error "oops"
    - so haskell has some side effects
    x = x 
    x = unsafePerformIO ...

    is also a side effect, because it will never terminate
    - pros: 
        - easy parallelism
        - there will never be a race condition
        - referential transperancy
        - never need to know whether a variable is a reference or the actual value
        - refactor easyily: you can always replace something with its defeinition

- lazy:
    - it only computes what it needs
    not strict 

    call-by-value: strict
    call-by-name vs call-by-need (?)

- statically-typeds:
    - Haskell has type inference
    - compiler has to check everything is well-typed


-}

-- call by name 
-- powerOfTwo 1 will only be computed once
powerOfTwo :: Integer -> Integer
powerOfTwo 0 = 1
powerOfTwo n = let x = powerOfTwo (n-1) in x + x

powerOfTwo' :: Integer -> Integer
powerOfTwo' 0 = 1
powerOfTwo' n = powerOfTwo (n-1) + powerOfTwo (n-1)


-- Int, Integer, Char, Bool
-- []

data Nat = Zero | Succ Nat
    deriving Show

one :: Nat
one = Succ Zero

two = Succ one

infinity :: Nat
infinity = Succ infinity

isZero Zero = True
isZero (Succ n) = False

instance Eq Nat where
    -- we need to define ==

    Zero == Zero = True
    Succ n == Succ m = n == m
    _ == _ = False


instance Ord Nat where
    -- needs to be of Eq
    Zero <= _ = True
    Succ _ <= Zero = False
    Succ n <= Succ m = n <= m

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- length :: [a] -> Int
goodLength :: [a] -> Nat

goodLength [] = Zero
goodLength (_ : xs) = Succ (goodLength xs)


-- isInfixOf [2,3] [1..4] == True 


-- isInfixOf' :: [a] -> [a] 


tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf (_:_) [] = False
isPrefixOf (x:xs) (y:ys)
    | x == y = isPrefixOf xs ys
    | otherwise = False


isInfixOf :: Eq a => [a] -> [a] -> Bool 
isInfixOf xs ys = any (isPrefixOf xs) (tails ys)