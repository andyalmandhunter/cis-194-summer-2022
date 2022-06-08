module Homework.Week05.Assignment
  ( eval
  , evalStr
  , ExprT(..)
  , Expr(..)
  , MinMax(..)
  -- uncomment these once you've defined them:
  -- Mod7(..)
  ) where

import           Homework.Week05.ExprT
import           Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval (Lit x  ) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- #2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- #3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- #4
instance Expr Integer where
  lit = id
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min
