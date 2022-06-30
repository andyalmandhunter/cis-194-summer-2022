module Homework.Week08.Assignment
  ( first
  , abParser
  , abParser_
  , intPair
  , intOrUppercase
  ) where

import           Control.Applicative            ( Alternative((<|>), empty) )
import           Control.Monad                  ( void )
import           Homework.Week08.AParser        ( Parser(Parser)
                                                , char
                                                , posInt
                                                )

-- #1
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f (Parser g) = Parser (fmap (first f) . g)

-- #2
instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (Parser a) <*> (Parser b) =
    let f Nothing       = Nothing
        f (Just (x, s)) = g x (a s)
        g _ Nothing       = Nothing
        g x (Just (h, s)) = Just (h x, s)
    in  Parser (f . b)

-- #3
abParser :: Parser (Char, Char)
abParser = flip (,) <$> char 'b' <*> char 'a'

abParser_ :: Parser ()
abParser_ = void $ char 'b' <* char 'a'

intPair :: Parser [Integer]
intPair = f <$> posInt <* char ' ' <*> posInt where f x y = [y, x]

-- #4
instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser a) <|> (Parser b) = Parser f where f s = a s <|> b s

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
