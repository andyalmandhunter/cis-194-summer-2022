{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Homework.Week07.Sized where

import           Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

instance Monoid Size where
  mempty = Size 0

instance Semigroup Size where
  Size s1 <> Size s2 = Size (s1 + s2)
