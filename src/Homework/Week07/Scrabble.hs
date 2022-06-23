module Homework.Week07.Scrabble where

import           Data.Char                      ( toLower )

newtype Score = Score Int
  deriving (Show, Eq)

instance Semigroup Score where
  Score m1 <> Score m2 = Score (m1 + m2)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score =
  let go 'a' = 1
      go 'b' = 3
      go 'c' = 3
      go 'd' = 2
      go 'e' = 1
      go 'f' = 4
      go 'g' = 2
      go 'h' = 4
      go 'i' = 1
      go 'j' = 8
      go 'k' = 5
      go 'l' = 1
      go 'm' = 3
      go 'n' = 1
      go 'o' = 1
      go 'p' = 3
      go 'q' = 10
      go 'r' = 1
      go 's' = 1
      go 't' = 1
      go 'u' = 1
      go 'v' = 4
      go 'w' = 4
      go 'x' = 8
      go 'y' = 4
      go 'z' = 10
      go _   = 0
  in  Score . go . toLower

scoreString :: String -> Score
scoreString = mconcat . map score
