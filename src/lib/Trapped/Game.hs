{-# LANGUAGE TemplateHaskell #-}

module Trapped.Game
  ( startGame
  ) where

import Lens.Micro.Platform ( (+~), (%~), (^.), makeLenses, to )
import System.Random ( Random, randomRIO )


newtype Scandal = Scandal Int
  deriving (Eq, Num, Ord, Show)

newtype Obsession = Obsession Int
  deriving (Eq, Num, Ord, Show)

newtype FoodStores = FoodStores Int
  deriving (Eq, Num, Ord, Show)

newtype Roll = Roll Int
  deriving (Eq, Num, Ord, Random, Show)

data GameData = GameData
  { _gdScandal :: Scandal
  , _gdObsession :: Obsession
  , _gdFoodStores :: FoodStores
  , _gdRollHistory :: [Roll]
  }
  deriving Show

makeLenses ''GameData


adjustScandal :: Scandal -> GameData -> GameData
adjustScandal adj = gdScandal +~ adj


adjustObsession :: Obsession -> GameData -> GameData
adjustObsession adj = gdObsession +~ adj


adjustFoodStores :: FoodStores -> GameData -> GameData
adjustFoodStores adj = gdFoodStores +~ adj


initialData :: GameData
initialData = GameData (Scandal 0) (Obsession 10) (FoodStores 10) []

data State
  = Playing GameData
  | Ended
  deriving Show

newtype Message = Message String
  deriving Show


gameContinuing :: State -> IO State
gameContinuing st@Ended = pure st
gameContinuing st@(Playing gd)
  | gd ^. gdScandal >= Scandal 10 = displayMsg (Message "The pressure and mockery from society reaches such heights that you decide to sell the house. Hans moves in after you leave and squats there.") >> pure Ended
  | gd ^. gdObsession < Obsession 1 = displayMsg (Message "Hans finally loses interest in you and finds another unattainable person to chase. Victory. He does, however, write a thinly veiled short story about you. It's not flattering.") >> pure Ended
  | gd ^. gdFoodStores < FoodStores 1 = displayMsg (Message "You run out of food and starve before Hans relents. The funeral is tasteful. He does not attend.") >> pure Ended
  | otherwise = pure st


hasItBeenFiveWeeks :: [Roll] -> Bool
hasItBeenFiveWeeks l = length l >= 3 && (all (== Roll 5) . take 3 $ l)


rollDie :: State -> IO State
rollDie st@Ended = pure st
rollDie (Playing gd) = do
  newRoll <- randomRIO (Roll 1, Roll 6)
  let newData = gdRollHistory %~ (newRoll :) $ gd
  if hasItBeenFiveWeeks $ newData ^. gdRollHistory
    then displayMsg (Message "Victory. Hans is dragged away kicking and screaming by an apologetic relative, nurse or member of the constabulary.") >> pure Ended
    else pure $ Playing newData


-- FIXME
ynChoice :: Message -> IO Bool
ynChoice (Message s) = do
  putStrLn s
  pure True


displayMsg :: Message -> IO ()
displayMsg (Message s) = putStrLn s


evalEvent :: State -> IO State

evalEvent st@Ended = pure st

evalEvent st@(Playing gd)
  | gd ^. gdRollHistory . to head <= Roll 2 = rollDie st >>= desperationLookup
  | otherwise = rollDie st >>= desperationLookup


desperationLookup :: State -> IO State
desperationLookup st@Ended = pure st
desperationLookup (Playing gd) = do
  Playing <$> case gd ^. gdRollHistory . to head of
    Roll 1 -> do
      displayMsg (Message "Rats. Rats in your basement")
      pure $ (adjustFoodStores $ FoodStores (-1)) gd
    Roll 2 -> do
      displayMsg (Message "Your dogs have eyes as big as saucers. Or dinner plates.")
      pure $ (adjustFoodStores $ FoodStores (-2)) gd
    Roll 3 -> do
      eatTheDuckling <- ynChoice $ Message "You find an ugly duckling. It's sad. Do you eat it?"
      if eatTheDuckling
        then displayMsg (Message "That was delicious!") >> (pure $ (adjustFoodStores $ FoodStores 1) gd)
        else displayMsg (Message "Another mouth to feed.") >> (pure $ (adjustFoodStores $ FoodStores (-1)) gd)
    Roll 4 -> do
      displayMsg (Message "You write letters to your friends. No help comes.")
      pure $ (adjustScandal $ Scandal 1) gd
    Roll 5 -> displayMsg (Message "You play solitaire.") >> pure gd
    Roll 6 -> do
      displayMsg (Message "More of your food spoils. How long can this go on?")
      pure $ (adjustFoodStores $ FoodStores (-1)) gd
    _ -> displayMsg (Message "not yet implemented") >> pure gd


startGame :: IO ()
startGame = gameLoop . Playing $ initialData


gameLoop :: State -> IO ()

gameLoop st@(Playing _) = do
  putStrLn "---"
  newState <- rollDie st >>= evalEvent >>= gameContinuing
  print newState
  gameLoop newState

gameLoop Ended = pure ()
