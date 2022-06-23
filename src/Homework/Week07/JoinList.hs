{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module Homework.Week07.JoinList
  ( tag
  , indexJ
  , (+++)
  , (!!?)
  , jlToList
  , dropJ
  , takeJ
  , scoreLine
  , Sized(..)
  , JoinList(..)
  ) where

import           Homework.Week07.Buffer
import           Homework.Week07.Scrabble
import           Homework.Week07.Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _  ) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0  = Just x
(x : xs) !!? i  = xs !!? (i - 1)

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ x  ) = [x]
jlToList (Append _ x y) = jlToList x ++ jlToList y

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ x) | i == 0    = Just x
                      | otherwise = Nothing
indexJ i (Append _ x y) | i < xSize = indexJ i x
                        | otherwise = indexJ (i - xSize) y
  where xSize = getSize (size (tag x))

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl    = jl
dropJ i (Single s x) | i > 0     = Empty
                     | otherwise = Single s x
dropJ i (Append s x y)
  | i < xSize = let x' = dropJ i x in Append (tag x' <> tag y) x' y
  | otherwise = dropJ (i - xSize) y
  where xSize = getSize (size (tag x))

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty        = Empty
takeJ i _ | i < 1    = Empty
takeJ i (Single s x) = Single s x
takeJ i (Append s x y)
  | i < xSize = takeJ i x
  | otherwise = let y' = takeJ (i - xSize) y in Append (tag x <> tag y') x y'
  where xSize = getSize (size (tag x))

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-- Type class for data structures that can represent the text buffer
-- of an editor.
instance Buffer (JoinList (Score, Size) String) where

  -- | Convert a buffer to a String.
  toString :: JoinList (Score, Size) String -> String
  toString jl =
    let go Empty          = []
        go (Single _ x  ) = [x]
        go (Append _ x y) = go x ++ go y
    in  unlines $ go jl

  -- | Create a buffer from a String.
  fromString :: String -> JoinList (Score, Size) String
  fromString = foldr f Empty . lines
   where
    f x acc =
      let x' = Single (scoreString x, Size $ length x) x
      in  Append (tag x' <> tag acc) x' acc

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line _ Empty = Nothing
  line i (Single _ x) | i == 0    = Just x
                      | otherwise = Nothing
  line 0 (Append _ x _) = line 0 x
  line i (Append _ _ y) = line (i - 1) y

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine
    :: Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine _ _ Empty = Empty
  replaceLine i s (Single _ _)
    | i == 0    = Single (scoreString s, Size $ length s) s
    | otherwise = Empty
  replaceLine 0 s (Append _ x y) =
    let x' = replaceLine 0 s x in Append (tag x' <> tag y) x' y
  replaceLine i s (Append _ x y) =
    let y' = replaceLine (i - 1) s y in Append (tag x <> tag y') x y'

  -- | Compute the number of lines in the buffer.
  numLines :: JoinList (Score, Size) String -> Int
  numLines = undefined

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value :: JoinList (Score, Size) String -> Int
  value = undefined
