{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week11.Assignment where

import           Control.Monad.Random           ( MonadRandom(getRandom)
                                                , Rand
                                                , Random(random, randomR)
                                                , StdGen
                                                , replicateM
                                                )

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
wins :: [DieValue] -> [DieValue] -> Int
wins = undefined

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  a <- replicateM (attackers bf - 1) die
  d <- replicateM (defenders bf) die
  return $ Battlefield (attackers bf - wins a d) (defenders bf - wins d a)

-- #3
invade :: Battlefield -> Rand StdGen Battlefield
invade = undefined

-- #4
successProb :: Battlefield -> Rand StdGen Double
successProb = undefined

-- #5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
