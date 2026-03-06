module Hf4 where
import Distribution.Simple.Register (inplaceInstalledPackageInfo)

separatePrint :: IO ()
-- separatePrint = do 
--     inp <- getLine
--     -- splitLines <- splitThrees inp 
--     -- not like this. each step has to return io
--     foldMap print $ splitThrees inp
--         where 
--             splitThrees :: String -> [String]
--             splitThrees s = case s of 
--                 (x:y:z:xs) -> [x,y,z] : splitThrees xs
--                 xs -> [xs]

separatePrint = 
    getLine >>= \inp ->
    foldMap print $ splitThrees inp
        where
            splitThrees :: String -> [String]
            splitThrees s = case s of 
                (x:y:z:xs) -> [x,y,z] : splitThrees xs
                xs -> [xs]



guessingGame :: Integer -> Integer -> IO ()
guessingGame l u = 
    if l == u 
    then print l
    else 
        let half = (u + l) `div` 2 in 
        print half >>
        getLine >>= \ans ->
        case ans of
            "E" -> print half
            "G" -> guessingGame (half+1) u
            "L" -> guessingGame l (half-1)
            _ -> fail "Answer must be 'G' for greater, 'L' for lower, 'E' for exact guess."


-- do notation
--     if l == u 
--     then print l
--     else do
--         let half = (u + l) `div` 2
--         _ <- print half
--         -- print half >>= \_ -> 
--         ans <- getLine
--         case ans of
--             "E" -> print half
--             "G" -> guessingGame (half+1) u
--             "L" -> guessingGame l (half-1)
--             _ -> fail "Answer must be 'G' for greater, 'L' for lower, 'E' for exact guess."
            