module Test.Examples.Snake.Model where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.Examples.Snake.Core (Goodie(..), Maze(..), Snake(..))
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Vector (Vector)

data Msg
  = Msg_Start
  | Msg_Pause
  | Msg_Resume
  | Msg_Tick
  | Msg_Navigate Direction

data State
  = Sta_Init
  | Sta_Playing Game
  | Sta_Pause Game
  | Sta_Lost Score
  | Sta_Won Score
  | Sta_Error String

derive instance Generic State _

derive instance Eq State

instance Show State where
  show = genericShow

newtype Game = Game
  { snake :: Snake
  , maze :: Maze
  , score :: Score
  , goodie :: Goodie
  , direction :: Direction
  }

derive instance Generic Game _

derive instance Eq Game

instance Show Game where
  show = genericShow

newtype Score = Score Int


derive instance Generic Score _

derive instance Eq Score

instance Show Score where
  show = genericShow
