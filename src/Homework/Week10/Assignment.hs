module Homework.Week10.Assignment
      ( treeFromList
      , sample
      ) where

import           Control.Monad                  ( replicateM )
import           Homework.Week10.Support        ( Tree(..)
                                                , labelTree
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , Gen
                                                , choose
                                                , sample
                                                , sized
                                                )

-- Exercise 1

insert :: a -> Tree a -> Tree a
insert x = Node (Leaf x)

treeFromList :: [a] -> Tree a
treeFromList []       = error "List must not be empty"
treeFromList (x : xs) = foldr insert (Leaf x) xs

genTree :: Arbitrary a => Gen (Tree a)
genTree = sized $ \size -> do
      len  <- choose (1, size)
      vals <- replicateM len arbitrary
      return $ treeFromList vals

instance Arbitrary a => Arbitrary (Tree a) where
      arbitrary = genTree

-- Exercise 2

size :: Tree a -> Int
size (Leaf _  ) = 1
size (Node a b) = size a + size b

toList :: Tree a -> [a]
toList (Leaf x  ) = [x]
toList (Node x y) = toList x ++ toList y

-- Exercise 3

-- The length of the list produced by toList is the size of the given tree.
prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList = undefined

-- labelTree does not change the size of the tree.
prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree = undefined

-- For every tree t, toList (labelTree t) is the expected list.
-- Hint: [0..n] denotes the list of numbers from 0 to n, inclusively.
prop_labelTree :: Tree Integer -> Bool
prop_labelTree = undefined

-- Applying labelTree to a list twice does yield the same list as applying it once.
prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent = undefined
