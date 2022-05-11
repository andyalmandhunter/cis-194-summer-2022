module Homework.Week01.Assignment where

-- #1a
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

rev :: [Integer] -> [Integer]
rev = foldl (\acc n -> n:acc) []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = rev . doubleEveryOtherRev . rev

-- #3
toDigitsList :: [Integer] -> [Integer]
toDigitsList = foldl (\acc n -> acc ++ toDigits n) []

sumDigits :: [Integer] -> Integer
sumDigits = foldl (+) 0 . toDigitsList

-- #4
validate :: Integer -> Bool
validate n =
  let sumDoubledDigits = sumDigits . doubleEveryOther . toDigits
  in mod (sumDoubledDigits n) 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
