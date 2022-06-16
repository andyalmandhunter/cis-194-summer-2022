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
    in  "[" ++ go s ++ ",..]"

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) = Stream (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-- #5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) y = Stream x (interleaveStreams y xs)


interleaveStreams (Stream x xs) (Stream y ys) =
  Stream x $ Stream y $ interleaveStreams xs ys

ruler :: Stream Integer
ruler = go 0 where go n = interleaveStreams (streamRepeat n) (go (n + 1))

-- ruler = interleaveStreams
--   (streamRepeat 0)
--   (interleaveStreams
--     (streamRepeat 1)
--     (interleaveStreams (streamRepeat 2)
--                        (interleaveStreams (streamRepeat 3) (streamRepeat 4))
--     )
--   )
