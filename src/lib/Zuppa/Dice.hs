module Zuppa.Dice
  ( Roll (..)
  , RollD6
  , RollD20
  , rollDie
  ) where

import System.Random.Stateful ( globalStdGen, uniformRM )


-- General dice type and type class

class Rollable a where
  rollDie :: IO a

data Roll d = Roll Int
  deriving (Eq, Ord, Show)


-- 6-sided dice

data D6

instance Rollable (Roll D6) where
  rollDie :: IO (Roll D6)
  rollDie = Roll <$> uniformRM (1, 6) globalStdGen

type RollD6 = Roll D6


-- 20-sided dice

data D20

instance Rollable (Roll D20) where
  rollDie :: IO (Roll D20)
  rollDie = Roll <$> uniformRM (1, 20) globalStdGen

type RollD20 = Roll D20
