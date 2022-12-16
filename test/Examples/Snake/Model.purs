module Test.Examples.Snake.Model where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.Examples.Snake.Core (Maze(..), Snake(..))
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Vector (Vector)

data Msg
  = Msg_Start
  | Msg_Pause
  | Msg_Tick
  | Msg_Navigate Direction

data State
  = Sta_Init
  | Sta_Playing Game
  | Sta_Pause Game
  | Sta_Lost Game
  | Sta_Won Game

derive instance Generic State _

instance Show State where
  show = genericShow

newtype Game = Game
  { snake :: Snake
  , maze :: Maze
  , score :: Int
  , goodie :: Vector Int
  , direction :: Direction
  }

derive instance Generic Game _

instance Show Game where
  show = genericShow