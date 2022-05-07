module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]

-- toDigits n
--   | n < 1     = []
--   | n < 10    = [n]
--   | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigits n = go n []
  where go m acc
          | m < 1     = []
          | m < 10    = m : acc
          | otherwise = go (div m 10) (mod m 10 : acc)
-- ^^ possible to express as a fold?

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1     = []
  | n < 10    = [n]
  | otherwise = mod n 10 : toDigitsRev (div n 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = undefined

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = undefined

-- #4
validate :: Integer -> Bool
validate = undefined

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
