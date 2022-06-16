module Homework.Week06.Assignment
  ( fib
  , fibs1
  , fibs2
  , streamToList
  , streamRepeat
  , streamMap
  , streamFromSeed
  , nats
  , ruler
  , Stream(..)
  ) where

import           Data.List                      ( intercalate )

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- #2
fibs2 :: [Integer]
fibs2 = let f (x, y) = (y, x + y) in map fst $ iterate f (0, 1)

-- #3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show s =
    let go = intercalate "," . map show . take 20 . streamToList
    in  "[" ++ go s ++ ",...]"

-- #4
streamRepeat :: a -> Stream a
streamRepeat = undefined

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = undefined

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed = undefined

-- #5
nats :: Stream Integer
nats = undefined

ruler :: Stream Integer
ruler = undefined
