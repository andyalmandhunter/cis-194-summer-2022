module Homework.Week01.Assignment where

-- #1a

-- toDigits n
--   | n < 1     = []
--   | n < 10    = [n]
--   | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigits :: Integer -> [Integer]
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
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []       = []
doubleEveryOtherRev [n]      = [n]
doubleEveryOtherRev (n:m:ns) = n : (2*m) : doubleEveryOtherRev ns

-- revDigits ns = go ns []
--   where
--     go [] acc = acc
--     go (m:ms) acc = go ms (m:acc)

revDigits :: [Integer] -> [Integer]
revDigits ns = foldl (\acc n -> n:acc) [] ns

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = revDigits (doubleEveryOtherRev (revDigits ns))

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
