{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Trapped.Game
  ( startGame
  ) where

-- import Control.Monad ( replicateM )
-- import Lens.Micro.Platform ( makeLenses )
import Lens.Micro.Platform
import System.Random ( Random, randomRIO )


newtype Scandal = Scandal Int
  deriving (Eq, Num, Ord, Show)

newtype Obsession = Obsession Int
  deriving (Eq, Num, Ord, Show)

newtype FoodStores = FoodStores Int
  deriving (Eq, Num, Ord, Show)

newtype Roll = Roll Int
  deriving (Eq, Num, Random, Show)

data GameData = GameData
  { _gdScandal :: Scandal
  , _gdObsession :: Obsession
  , _gdFoodStores :: FoodStores
  , _gdRollHistory :: [Roll]
  }
  deriving Show

makeLenses ''GameData

adjustFoodStores :: FoodStores -> GameData -> GameData
adjustFoodStores fs gd = gd { gdFoodStores + fs }

initialState :: GameData
initialState = GameData (Scandal 0) (Obsession 10) (FoodStores 10) []

newtype Description = Description String
  deriving Show


gameContinuing :: GameData -> Either Description GameData
gameContinuing gd
  | gd ^. gdScandal >= Scandal 10 = Left (Description "The pressure and mockery from society reaches such heights that you decide to sell the house. Hans moves in after you leave and squats there.")
  | gd ^. gdObsession < Obsession 1 = Left (Description "Hans finally loses interest in you and finds another unattainable person to chase. Victory. He does, however, write a thinly veiled short story about you. It's not flattering.")
  | gd ^. gdFoodStores < FoodStores 1 = Left (Description "You run out of food and starve before Hans relents. The funeral is tasteful. He does not attend.")
  | otherwise = Right gd


checkFiveWeekRule :: GameData -> Either String GameData
checkFiveWeekRule gd =
  if all (== Roll 5) . take 3 $ gd ^. gdRollHistory
    then Left "Victory. Hans is dragged away kicking and screaming by an apologetic relative, nurse or member of the constabulary."
    else Right gd


getRoll :: IO Roll
getRoll = randomRIO (Roll 1, Roll 6)


-- adjustState :: _ -> GameData -> GameData

{- Each roll starts a series of look ups in the tables

   First in the Hans On Event... table, which direct to one of the other three
   Desperation Mounts..., Peeking Through The Curtains..., Hans' Antics...
-}


data Adjustment = Adjustment Description [GameData -> GameData]


makeChoice :: Description -> IO Bool


hansEventLookup = \case
  1 ->
  2 -> desperationLookup

desperationLookup :: Roll -> IO LookupAction
desperationLookup = \case
  Roll 1 -> pure . Adjustment
    (Description "Rats. Rats in your basement")
    [ gdFoodStores %~ (subtract (FoodStores 1)) ]
  Roll 2 -> pure . Adjustment
    (Description "Your dogs have eyes as big as saucers. Or dinner plates.")
    [ gdFoodStores %~ (subtract (FoodStores 2)) ]
  Roll 3 -> do
    eatTheDuckling <- makeChoice $ Description "You find an ugly duckling. It's sad. Do you eat it?"
    pure . if eatTheDuckling
      then Adjustment (Description "That was delicious!") [ adjustFoodStores +1 ]
      else Adjustment (Description "Another mouth to feed!") [ adjustFoodStores -1 ]
    Choice (Description "You find an ugly duckling. It's sad. Do you eat it?")
  _ -> Adjustment (Description "not yet implemented") []


startGame :: IO ()
startGame = do
  print $ gameContinuing initialState
  print . gameContinuing . (gdScandal .~ Scandal 10) $ initialState
  -- print =<< replicateM 6 getRoll
