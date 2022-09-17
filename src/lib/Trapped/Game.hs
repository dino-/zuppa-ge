{-# LANGUAGE LambdaCase #-}

module Trapped.Game
  ( startGame
  ) where

import Control.Monad ( replicateM )
import System.Random ( Random, randomRIO )


newtype Scandal = Scandal Int
  deriving (Eq, Num, Ord, Show)

newtype Obsession = Obsession Int
  deriving (Eq, Num, Ord, Show)

newtype FoodStores = FoodStores Int
  deriving (Eq, Num, Ord, Show)

newtype Roll = Roll Int
  deriving (Eq, Num, Random, Show)

data GameState = GameState
  { gsScandal :: Scandal
  , gsObsession :: Obsession
  , gsFoodStores :: FoodStores
  , gsRollHistory :: [Roll]
  }
  deriving Show

initialState :: GameState
initialState = GameState (Scandal 0) (Obsession 10) (FoodStores 10) []


gameContinuing :: GameState -> Either String GameState
gameContinuing gs
  | gsScandal gs >= Scandal 10 = Left "The pressure and mockery from society reaches such heights that you decide to sell the house. Hans moves in after you leave and squats there."
  | gsObsession gs < Obsession 1 = Left "Hans finally loses interest in you and finds another unattainable person to chase. Victory. He does, however, write a thinly veiled short story about you. It's not flattering."
  | gsFoodStores gs < FoodStores 1 = Left "You run out of food and starve before Hans relents. The funeral is tasteful. He does not attend."
  | otherwise = Right gs


checkFiveWeekRule :: GameState -> Either String GameState
checkFiveWeekRule gs =
  if all (== Roll 5) . take 3 . gsRollHistory $ gs
    then Left "Victory. Hans is dragged away kicking and screaming by an apologetic relative, nurse or member of the constabulary."
    else Right gs


getRoll :: IO Roll
getRoll = randomRIO (Roll 1, Roll 6)


-- adjustFoodStores :: FoodStores -> GameState -> GameState


-- desperationLookup :: Roll -> _
-- desperationLookup = \case
--   Roll 1 -> ("Rats. Rats in your basement", subtract (FoodStores 1))


startGame :: IO ()
startGame = do
  print $ gameContinuing initialState
  print . gameContinuing $ initialState { gsScandal = Scandal 10 }
  print =<< replicateM 6 getRoll
