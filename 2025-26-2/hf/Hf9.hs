{-# LANGUAGE LambdaCase #-}


module Hf9 where

import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Traversable
import Data.Foldable
import Data.Char
import Data.Maybe


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } deriving Functor
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap
instance Monad Parser where
  (Parser rP) >>= f = Parser $ \s -> case rP s of
    Nothing -> Nothing
    Just (a, s') -> runParser (f a) s'

-- Primitív parserek
-- Olyan parser, amely lenyel egy karaktert a bemenetről és akkor fogad el, ha az adott predikátum teljesül rá
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  [] -> Nothing
  (c:xs) -> if p c then Just (c, xs) else Nothing


char :: Char -> Parser ()
-- char c = Parser $ \inp -> satisfy (== c) inp -- nem jó
-- char c = const () <$> satisfy (== c)
char c = () <$ satisfy (== c) -- vagy void
-- char c = void $ satisfy (== c) -- vagy ez

-- Parseoljunk akármilyen karaktert
anychar :: Parser Char
anychar = satisfy (const True)


-- Parsernél fontos a "vagy" művelet (ha az első parser elhasal, akkor a másikat próbáljuk meg)
-- Ez lesz az Alternative típusosztály
-- hasonló az Applicative-hoz
instance Alternative Parser where
  empty :: Parser a -- Garantáltan elhasaló parser
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a -- Ha a baloldali sikertelen, futassuk le a jobboldalit (hint: A Maybe is egy alternatív)
  Parser l <|> Parser r = Parser $ \input -> case l input of
    Just res -> Just res
    Nothing -> r input





ph1, ph2, ph3 :: Parser ()


-- (c+)(i?)(c{10,})a
ph1 = 
    some (char 'c') >>
    optional (char 'i') >>
    replicateM_ 10 (char 'c') >>
    many (char 'c') >>
    char 'a'


-- ((a+)|ab.)(b*)
ph2 = 
    (
        void (some (char 'a'))
        <|>
        (
            char 'a' >>
            char 'b' >>
            void anychar
        )
    ) >>
    void (many (char 'b'))

-- ([a,b,c]{5}|b+)|((a.)+)
ph3 = 
    (
        replicateM_ 5 (satisfy (\c -> c >= 'a' && c <= 'c'))
        <|>
        void (some (char 'b'))
    )
    <|>
    void (some (char 'a' >> anychar))