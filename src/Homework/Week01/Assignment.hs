module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n =
  let loop m acc
        | m < 1     = acc
        | m < 10    = m : acc
        | otherwise = loop (div m 10) (mod m 10 : acc)
  in loop n []

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1     = []
  | n < 10    = [n]
  | otherwise = mod n 10 : toDigitsRev (div n 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther =
  let go []       = []
      go [n]      = [n]
      go (n:m:ns) = n : (2*m) : go ns
  in reverse . go . reverse

-- #3
toDigitsList :: [Integer] -> [Integer]
toDigitsList = foldl (\acc n -> acc ++ toDigits n) []

sumDigits :: [Integer] -> Integer
sumDigits = foldl (+) 0 . toDigitsList

-- #4
validate :: Integer -> Bool
validate n =
  let go = sumDigits . doubleEveryOther . toDigits
  in mod (go n) 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d = hanoi4 (n-2) a c b d ++ [(a ,d), (a, b), (d, b)] ++ hanoi4 (n-2) c b a d
