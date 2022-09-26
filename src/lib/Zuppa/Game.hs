{-# LANGUAGE TemplateHaskell #-}

module Zuppa.Game
  ( startGame
  ) where

import Lens.Micro.Platform ( (+~), (%~), (^.), makeLenses )
import Text.Printf ( printf )

import Zuppa.Dice ( Roll (..), RollD6, rollDie )


newtype Scandal = Scandal Int
  deriving (Eq, Num, Ord, Show)

newtype Obsession = Obsession Int
  deriving (Eq, Num, Ord, Show)

newtype FoodStores = FoodStores Int
  deriving (Eq, Num, Ord, Show)

data GameData = GameData
  { _gdScandal :: Scandal
  , _gdObsession :: Obsession
  , _gdFoodStores :: FoodStores
  , _gdRollHistory :: [RollD6]
  }
  deriving (Eq, Show)

makeLenses ''GameData


adjustScandal :: Int -> GameData -> GameData
adjustScandal adj = gdScandal +~ Scandal adj


adjustObsession :: Int -> GameData -> GameData
adjustObsession adj = gdObsession +~ Obsession adj


adjustFoodStores :: Int -> GameData -> GameData
adjustFoodStores adj = gdFoodStores +~ FoodStores adj


initialData :: GameData
initialData = GameData (Scandal 0) (Obsession 10) (FoodStores 10) []


data GameState
  = SInitGame
  | SStartTurn GameData
  | SDesperation GameData
  | SPeeking GameData
  | SAntics GameData
  | SEnded
  deriving (Show, Eq)

data GameEvent
  = ERoll RollD6
  | EEndConditions
  deriving (Show, Eq)


addRoll :: RollD6 -> GameData -> GameData
addRoll roll gd = (gdRollHistory %~ (roll :)) gd


evalGame :: GameState -> GameEvent -> IO GameState

evalGame SInitGame (ERoll roll) =
  pure . SStartTurn . addRoll roll $ initialData

evalGame (SStartTurn gd) (ERoll roll) = do
  let stateConstructor = case roll of
        Roll 1 -> SDesperation
        Roll 2 -> SDesperation
        Roll 3 -> SPeeking
        Roll 4 -> SPeeking
        Roll 5 -> SAntics
        Roll 6 -> SAntics
        _ -> SDesperation
  pure . stateConstructor . addRoll roll $ gd

evalGame (SDesperation gd) (ERoll roll) = do
  newGd <- desperationLookup gd roll
  pure $ SStartTurn newGd

evalGame (SPeeking gd) (ERoll roll) = do
  newGd <- peekingLookup gd roll
  pure $ SStartTurn newGd

evalGame (SAntics gd) (ERoll roll) = do
  newGd <- anticsLookup gd roll
  pure $ SStartTurn newGd

-- We can end the game from any state
evalGame _ EEndConditions = pure SEnded

evalGame state _ = pure state


display :: String -> IO ()
display = putStrLn


displayGameData :: GameData -> IO ()
displayGameData (GameData (Scandal scandal) (Obsession obsession) (FoodStores foodStores) _) =
  printf "Scandal: %2d  Obsession: %2d  Food stores: %2d\n"
    scandal obsession foodStores


-- FIXME
ynChoice :: String -> IO Bool
ynChoice s = do
  putStrLn s
  pure True


desperationLookup :: GameData -> RollD6 -> IO GameData
desperationLookup oldGd roll = do
  display "---"
  display "Desperation mounts..."
  let gd = addRoll roll oldGd
  (msg, adjustedGd) <- case roll of
    Roll 1 -> pure
      ( "Rats. Rats in your basement"
      , adjustFoodStores (-1) gd )
    Roll 2 -> pure
      ( "Your dogs have eyes as big as saucers. Or dinner plates."
      , adjustFoodStores (-2) gd )
    Roll 3 -> do
      eatTheDuckling <- ynChoice $ "You find an ugly duckling. It's sad. Do you eat it?"
      pure $ if eatTheDuckling
        then ("That was delicious!", adjustFoodStores 1 gd)
        else ("Another mouth to feed.", adjustFoodStores (-1) gd)
    Roll 4 -> pure
      ( "You write letters to your friends. No help comes."
      , adjustScandal 1 gd )
    Roll 5 -> pure ("You play solitaire.", gd)
    Roll 6 -> pure
      ( "More of your food spoils. How long can this go on?"
      , adjustFoodStores (-1) gd )
    _ -> pure ("impossible case", gd)
  display msg
  displayGameData adjustedGd
  pure adjustedGd


peekingLookup :: GameData -> RollD6 -> IO GameData
peekingLookup oldGd roll = do
  display "---"
  display "Peeking through the curtains..."
  let gd = addRoll roll oldGd
  let (msg, adjustedGd) = case roll of
        Roll 1 ->
          ( "A little matchstick girl dies on your lawn. In front of him."
          , (adjustObsession (-1)) . (adjustScandal 1) $ gd )
        Roll 2 ->
          ( "He's having a portrait done of himself. On your LAWN."
          , adjustScandal 1 gd )
        Roll 3 ->
          ( "He's hired a small orchestra to blast music at your house."
          , adjustScandal 2 gd )
        Roll 4 ->
          ( "A new face wanders past."
          , adjustObsession (-1) gd )
        Roll 5 ->
          ( "He's talking to your neighbours about you."
          , adjustObsession 1 gd )
        Roll 6 ->
          ( "He catches a glimpse of you."
          , adjustObsession 1 gd )
        _ -> ("impossible case", gd)
  display msg
  displayGameData adjustedGd
  pure adjustedGd


anticsLookup :: GameData -> RollD6 -> IO GameData
anticsLookup oldGd roll = do
  display "---"
  display "Hans' antics..."
  let gd = addRoll roll oldGd
  let (msg, adjustedGd) = case roll of
        Roll 1 ->
          ( "He's naked. He claims the clothes are visible."
          , adjustScandal 2 gd )
        Roll 2 ->
          ( "He's shoving letters through your door. Stack of them."
          , adjustObsession (-1) gd )
        Roll 3 ->
          ( "He's weeping and howling, rolling about on the grass."
          , adjustScandal 1 . adjustObsession (-1) $ gd )
        Roll 4 ->
          ( "He stamps on a parcel intended for you."
          , adjustFoodStores (-1) gd )
        Roll 5 ->
          ( "He's punching the house and crying about it."
          , adjustObsession (-1) gd )
        Roll 6 ->
          ( "He puts his mouth to the letterbox and screams."
          , adjustScandal 1 gd )
        _ -> ("impossible case", gd)
  display msg
  displayGameData adjustedGd
  pure adjustedGd


startGame :: IO ()
startGame = gameLoop SInitGame


hasItBeenFiveWeeks :: [RollD6] -> Bool
hasItBeenFiveWeeks rolls = length rolls >= 3 && (all (== Roll 5) . take 3 $ rolls)


endConditionsMet :: GameData -> IO Bool
endConditionsMet gd
  | gd ^. gdScandal >= Scandal 10 = do
    display "---"
    display "The pressure and mockery from society reaches such heights that you decide to sell the house. Hans moves in after you leave and squats there."
    displayGameData gd
    pure True
  | gd ^. gdObsession < Obsession 1 = do
    display "---"
    display "Hans finally loses interest in you and finds another unattainable person to chase. Victory. He does, however, write a thinly veiled short story about you. It's not flattering."
    displayGameData gd
    pure True
  | gd ^. gdFoodStores < FoodStores 1 = do
    display "---"
    display "You run out of food and starve before Hans relents. The funeral is tasteful. He does not attend."
    displayGameData gd
    pure True
  | hasItBeenFiveWeeks $ gd ^. gdRollHistory = do
    display "---"
    display "After five harrowing weeks, victory. Hans is dragged away kicking and screaming by an apologetic relative, nurse or member of the constabulary."
    displayGameData gd
    pure True
  | otherwise = pure False


checkAndRoll :: GameData -> IO (Maybe GameEvent)
checkAndRoll gd = do
  ec <- endConditionsMet gd
  if ec
    then pure $ Just EEndConditions
    else Just . ERoll <$> rollDie


gameLoop :: GameState -> IO ()
gameLoop state = do
  -- Compute the next GameEvent based on the current GameState and analysis of the GameData
  mevent <- case state of
    SInitGame -> Just . ERoll <$> rollDie
    SStartTurn gd -> checkAndRoll gd
    SDesperation gd -> checkAndRoll gd
    SPeeking gd -> checkAndRoll gd
    SAntics gd -> checkAndRoll gd
    SEnded -> pure Nothing

  maybe (pure ()) (\event -> do
    newState <- evalGame state event
    gameLoop newState)
    mevent
