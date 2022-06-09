{-# LANGUAGE FlexibleInstances #-}

module Homework.Week05.Assignment
  ( eval
  , evalStr
  , ExprT(..)
  , Expr(..)
  , MinMax(..)
  , Mod7(..)
  , compile
  , stackVM
  ) where

import           Homework.Week05.ExprT          ( ExprT(..) )
import           Homework.Week05.Parser         ( parseExp )
import qualified Homework.Week05.StackVM       as StackVM
                                                ( StackExp(Add, Mul, PushI) )
import           Homework.Week05.StackVM        ( Program
                                                , stackVM
                                                )

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
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

-- #5
instance Expr Program where
  lit x = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
