module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]

-- toDigits n
--   | n < 1     = []
--   | n < 10    = [n]
--   | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigits n = go n []
  where go m acc | m < 1     = acc
                 | m < 10    = m : acc
                 | otherwise = go (div m 10) (mod m 10 : acc)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1     = []
  | n < 10    = [n]
  | otherwise = mod n 10 : toDigitsRev (div n 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [n]      = [n]
doubleEveryOther (n:m:ns) = n : (2*m) : doubleEveryOther ns

-- [1, 2, 3, 4] []
-- [2, 3, 4] [1]
-- [3, 4] [2, 1]
-- [4] [3, 2, 1]
-- [4, 3, 2, 1]

-- [2, 1] [6, 4]
-- [] [2, 2, 6, 4]

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
