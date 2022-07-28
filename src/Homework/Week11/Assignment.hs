{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week11.Assignment
  ( Battlefield(..)
  , battle
  , invade
  , successProb
  ) where

import           Control.Monad.Random           ( MonadRandom(getRandom)
                                                , Rand
                                                , Random(random, randomR)
                                                , StdGen
                                                , replicateM
                                                )
import           Data.List                      ( sort )

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield
  { attackers :: Army
  , defenders :: Army
  }
  deriving Show

-- #2 (there is no assignment #1, really)
attack :: (DieValue, DieValue) -> Bool
attack (DV x, DV y) = x > y

defend :: (DieValue, DieValue) -> Bool
defend (DV x, DV y) = y >= x

wins :: ((DieValue, DieValue) -> Bool) -> [DieValue] -> [DieValue] -> Int
wins p xs ys = length $ filter p $ zip (sort xs) (sort ys)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  a <- replicateM (min 3 (attackers bf - 1)) die
  d <- replicateM (min 2 (defenders bf)) die
  return $ Battlefield (attackers bf - wins attack a d)
                       (defenders bf - wins defend a d)

-- #3
won :: Battlefield -> Bool
won (Battlefield _ d) = d == 0

lost :: Battlefield -> Bool
lost (Battlefield a _) = a < 2

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  bf' <- battle bf
  if won bf' || lost bf' then return bf' else invade bf'

-- #4
fracSuccess :: [Battlefield] -> Double
fracSuccess bfs =
  fromIntegral (length $ filter won bfs) / fromIntegral (length bfs)

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  bfs <- replicateM 1000 (invade bf)
  return $ fracSuccess bfs

-- #5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
