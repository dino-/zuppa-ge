module Zuppa.Dice
  ( Roll (..)
  , rollDie
  ) where

import System.Random ( Random, randomRIO )


newtype Roll = Roll Int
  deriving (Eq, Num, Ord, Random, Show)


rollDie :: IO Roll
rollDie = randomRIO (Roll 1, Roll 6)
