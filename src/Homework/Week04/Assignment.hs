module Homework.Week04.Assignment (
  ex1,
  ex2,
  ex3,
  ex4,
  ex5,
  ex6,
  ex7,
  ex8,
  ex9,
  ex10,
  ex11,
  ex12,
  insertBST,
  allCaps,
  dropTrailingWhitespace,
  firstLetters,
  asList,
  BST(..)
) where

import Data.Char
import Data.Maybe
import Data.List
import Homework.Week04.BST

-- #1
ex1 :: a -> b -> b
ex1 _ x = x
-- Only one possible implementation: return the b we were passed.

-- #2
ex2 :: a -> a -> a
ex2 x _ = x
-- One other possible implementation:
--
--  ex2 _ x = x

-- #3
ex3 :: Int -> a -> a
ex3 _ x = x
-- Only possible implemention is to return the a we were passed.

-- #4
ex4 :: Bool -> a -> a -> a
ex4 True  x _ = x
ex4 False _ x = x
-- Many possible implementations, but this is the signature of an if-then-else
-- function so that's what I implemented.

-- #5
ex5 :: Bool -> Bool
ex5 True  = False
ex5 False = True
-- Several possible implementations, but one is `not`.

-- #6
ex6 :: (a -> a) -> a
ex6 = error "impossible"
-- No a is supplied so I can't return an a.

-- #7
ex7 :: (a -> a) -> a -> a
ex7 = ($)
-- This is the signature for `apply`. The only other possible implementation is
-- to ignore the function and return the a itself:  ex7 _ x = x

-- #8
ex8 :: [a] -> [a]
ex8 = reverse
-- Many possible implementations, but they can only depend on properties of the
-- list, not properties of a which is of unknown type. So something like sorting
-- isn't possible because we don't know if a is an Ord, but reversing or
-- skipping or returning the list unchanged are possible.

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map
-- This is the signature for `map`. I think the only other possible
-- implementation is to return the empty list:
--
--  ex9 _ _ = []

-- #10
ex10 :: Maybe a -> a
ex10 = error "impossible"
-- This is impossible to implement as a total function. A partial function is
-- possible:
--
--  ex10 (Just a) = a
--
-- But if the first argument is `Nothing`, we have no way to generate an a.

-- #11
ex11 :: a -> Maybe a
ex11 x = Just x
-- The other possible implementation is:
--
--  ex11 _ = Nothing

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id
-- The only possible implementation is the identity function.

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _   x Leaf         = Node Leaf x Leaf
insertBST cmp x (Node l y r) =
  let go LT x' y' l' r' = Node (insertBST cmp x' l') y' r'
      go _  x' y' l' r' = Node l'                    y' (insertBST cmp x' r')
  in go (cmp x y) x y l r

-- #14
allCaps :: [String] -> Bool
allCaps = all (fromMaybe False . fmap isUpper . listToMaybe)

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

-- #16
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe listToMaybe

-- #17
asList :: [String] -> String
asList x = "[" ++ (intercalate "," $ filter (not . null) x) ++ "]"
