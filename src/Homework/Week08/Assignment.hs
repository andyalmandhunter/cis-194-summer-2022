module Homework.Week08.Assignment
  ( first
  , abParser
  , abParser_
  , intPair
  , intOrUppercase
  ) where

import           Control.Applicative            ( Alternative((<|>), empty) )
import           Homework.Week08.AParser        ( Parser(Parser)
                                                , char
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
abParser_ = undefined

intPair :: Parser [Integer]
intPair = undefined

-- #4
instance Alternative Parser where
  empty = undefined
  _ <|> _ = undefined

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
