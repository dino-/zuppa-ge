{-# LANGUAGE TemplateHaskell #-}

module Trapped.Game
  ( startGame
  ) where

import Lens.Micro.Platform ( (+~), (%~), (^.), makeLenses, to )
import System.Random ( Random, randomRIO )
import Text.Printf ( printf )


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
  = ENewTurn Roll
  | EDesperation Roll
  | EPeeking Roll
  | EAntics Roll
  | EEndConditions
  deriving (Show, Eq)


type FSM s e = s -> e -> IO s


evalGame :: FSM GameState GameEvent

evalGame SInitGame (ENewTurn roll) =
  pure . SStartTurn . (gdRollHistory %~ (roll :)) $ initialData

evalGame (SStartTurn gd) (EDesperation roll) = do
  pure . SDesperation . (gdRollHistory %~ (roll :)) $ gd

evalGame (SStartTurn gd) (EPeeking roll) = do
  pure . SPeeking . (gdRollHistory %~ (roll :)) $ gd

evalGame (SStartTurn gd) (EAntics roll) = do
  pure . SAntics . (gdRollHistory %~ (roll :)) $ gd

evalGame (SDesperation gd) (ENewTurn roll) = do
  newGd <- desperationLookup gd roll
  pure $ SStartTurn newGd

evalGame (SPeeking gd) (ENewTurn roll) = do
  newGd <- peekingLookup gd roll
  pure $ SStartTurn newGd

evalGame (SAntics gd) (ENewTurn roll) = do
  newGd <- anticsLookup gd roll
  pure $ SStartTurn newGd

evalGame _ EEndConditions = pure SEnded

evalGame state _ = pure state


withLogging :: (Show s, Show e) => FSM s e -> FSM s e
withLogging fsm s e = do
  s' <- fsm s e
  printf "---\n%s x %s -> %s\n" (show s) (show e) (show s')
  return s'


newtype Message = Message String
  deriving Show


displayMsg :: Message -> GameData -> IO ()
displayMsg (Message s) (GameData (Scandal scandal) (Obsession obsession) (FoodStores foodStores) _) =
  printf "---\n%s\nScandal: %2d  Obsession: %2d  Food stores: %2d\n"
    s scandal obsession foodStores


-- FIXME
ynChoice :: Message -> IO Bool
ynChoice (Message s) = do
  putStrLn s
  pure True


desperationLookup :: GameData -> Roll -> IO GameData
desperationLookup oldGd roll = do
  let gd = gdRollHistory %~ (roll :) $ oldGd
  (msg, adjustedGd) <- case roll of
    Roll 1 -> pure
      ( Message "Rats. Rats in your basement"
      , adjustFoodStores (-1) gd )
    Roll 2 -> pure
      ( Message "Your dogs have eyes as big as saucers. Or dinner plates."
      , adjustFoodStores (-2) gd )
    Roll 3 -> do
      eatTheDuckling <- ynChoice $ Message "You find an ugly duckling. It's sad. Do you eat it?"
      pure $ if eatTheDuckling
        then (Message "That was delicious!", adjustFoodStores 1 gd)
        else (Message "Another mouth to feed.", adjustFoodStores (-1) gd)
    Roll 4 -> pure
      ( Message "You write letters to your friends. No help comes."
      , adjustScandal 1 gd )
    Roll 5 -> pure (Message "You play solitaire.", gd)
    Roll 6 -> pure
      ( Message "More of your food spoils. How long can this go on?"
      , adjustFoodStores (-1) gd )
    _ -> pure (Message "not yet implemented", gd)
  displayMsg msg adjustedGd
  pure adjustedGd


peekingLookup :: GameData -> Roll -> IO GameData
peekingLookup oldGd roll = do
  let gd = gdRollHistory %~ (roll :) $ oldGd
  let (msg, adjustedGd) = case roll of
        Roll 1 ->
          ( Message "A little matchstick girl dies on your lawn. In front of him."
          , (adjustObsession (-1)) . (adjustScandal 1) $ gd )
        Roll 2 ->
          ( Message "He's having a portrait done of himself. On your LAWN."
          , adjustScandal 1 gd )
        Roll 3 ->
          ( Message "He's hired a small orchestra to blast music at your house."
          , adjustScandal 2 gd )
        Roll 4 ->
          ( Message "A new face wanders past."
          , adjustObsession (-1) gd )
        Roll 5 ->
          ( Message "He's talking to your neighbours about you."
          , adjustObsession 1 gd )
        Roll 6 ->
          ( Message "He catches a glimpse of you."
          , adjustFoodStores 1 gd )
        _ -> (Message "not yet implemented", gd)
  displayMsg msg adjustedGd
  pure adjustedGd


anticsLookup :: GameData -> Roll -> IO GameData
anticsLookup oldGd roll = do
  let gd = gdRollHistory %~ (roll :) $ oldGd
  let (msg, adjustedGd) = case roll of
        Roll 1 ->
          ( Message "He's naked. He claims the clothes are visible."
          , adjustScandal 2 gd )
        Roll 2 ->
          ( Message "He's shoving letters through your door. Stack of them."
          , adjustObsession (-1) gd )
        Roll 3 ->
          ( Message "He's weeping and howling, rolling about on the grass."
          , adjustScandal 1 . adjustObsession (-1) $ gd )
        Roll 4 ->
          ( Message "He stamps on a parcel intended for you."
          , adjustFoodStores (-1) gd )
        Roll 5 ->
          ( Message "He's punching the house and crying about it."
          , adjustObsession (-1) gd )
        Roll 6 ->
          ( Message "He puts his mouth to the letterbox and screams."
          , adjustScandal 1 gd )
        _ -> (Message "not yet implemented", gd)
  displayMsg msg adjustedGd
  pure adjustedGd


rollDie :: IO Roll
rollDie = randomRIO (Roll 1, Roll 6)


startGame :: IO ()
startGame = gameLoop SInitGame


hasItBeenFiveWeeks :: [Roll] -> Bool
hasItBeenFiveWeeks rolls = length rolls >= 3 && (all (== Roll 5) . take 3 $ rolls)


endConditionsMet :: GameData -> IO Bool
endConditionsMet gd
  | gd ^. gdScandal >= Scandal 10 = displayMsg (Message "The pressure and mockery from society reaches such heights that you decide to sell the house. Hans moves in after you leave and squats there.") gd >> pure True
  | gd ^. gdObsession < Obsession 1 = displayMsg (Message "Hans finally loses interest in you and finds another unattainable person to chase. Victory. He does, however, write a thinly veiled short story about you. It's not flattering.") gd >> pure True
  | gd ^. gdFoodStores < FoodStores 1 = displayMsg (Message "You run out of food and starve before Hans relents. The funeral is tasteful. He does not attend.") gd >> pure True
  | hasItBeenFiveWeeks $ gd ^. gdRollHistory = displayMsg (Message "After five harrowing weeks, victory. Hans is dragged away kicking and screaming by an apologetic relative, nurse or member of the constabulary.") gd >> pure True
  | otherwise = pure False


gameLoop :: GameState -> IO ()
gameLoop state = do
  -- Compute the next GameEvent based on the current GameState and analysis of the GameData
  mevent <- case state of
    SInitGame -> Just . ENewTurn <$> rollDie
    SStartTurn gd -> do
      ec <- endConditionsMet gd
      if ec
        then pure $ Just EEndConditions
        else do
          let roll = gd ^. gdRollHistory . to head
          let eventConstructor = case roll of
                Roll 1 -> EDesperation
                Roll 2 -> EDesperation
                Roll 3 -> EPeeking
                Roll 4 -> EPeeking
                Roll 5 -> EAntics
                Roll 6 -> EAntics
                _ -> EDesperation
          pure . Just . eventConstructor $ roll
    SDesperation gd -> do
      ec <- endConditionsMet gd
      if ec
        then pure $ Just EEndConditions
        else Just . ENewTurn <$> rollDie
    SPeeking gd -> do
      ec <- endConditionsMet gd
      if ec
        then pure $ Just EEndConditions
        else Just . ENewTurn <$> rollDie
    SAntics gd -> do
      ec <- endConditionsMet gd
      if ec
        then pure $ Just EEndConditions
        else Just . ENewTurn <$> rollDie
    SEnded -> pure Nothing

  maybe (pure ()) (\event -> do
    -- newState <- (withLogging evalGame) state event
    newState <- evalGame state event
    gameLoop newState)
    mevent
