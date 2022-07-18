module Homework.Week08.Assignment
  ( first
  , abParser
  , abParser_
  , intPair
  , intOrUppercase
  ) where

import           Control.Applicative            ( Alternative((<|>), empty) )
import           Control.Monad                  ( void )
import           Data.Char                      ( isUpper )
import           Homework.Week08.AParser        ( Parser(Parser)
                                                , char
                                                , posInt
                                                , satisfy
                                                )

-- #1
instance Functor Parser where
  fmap f = (pure f <*>)

-- #2
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Applicative Parser where
  pure x = Parser f where f s = Just (x, s)
  (Parser a) <*> (Parser b) =
    let f Nothing       = Nothing
        f (Just (g, s)) = first g <$> b s
    in  Parser (f . a)

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void $ char 'a' <* char 'b'

intPair :: Parser [Integer]
intPair = f <$> posInt <* char ' ' <*> posInt where f x y = [x, y]

-- #4
instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser a) <|> (Parser b) = Parser f where f s = a s <|> b s

-- #5
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
