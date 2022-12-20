module Test.Examples.CountDown where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Marionett.Controllers.RecordAPI (ControlAPI)
import Marionett.Controllers.RecordAPI as RecAPI
import Marionette as Mar
import Marionette.Renderers.Commander as Comm

data State
  = Init
  | CountingDown Int
  | LaunchSpaceShip

derive instance Eq State

data Msg = Start | Tick

control :: ControlAPI Msg State -> Msg -> Aff Unit
control api = case _ of
  Start -> do
    api.modifyState_ case _ of
      Init -> CountingDown 10
      st -> st
    api.sendMsg Tick

  Tick -> do
    api.modifyState_ case _ of
      CountingDown 1 -> LaunchSpaceShip
      CountingDown n -> CountingDown (n - 1)
      st -> st

    Aff.delay (Milliseconds 1000.0)
    api.sendMsg Tick

view :: State -> Comm.Surface Msg
view = case _ of
  Init -> Comm.Surface
    ( Comm.TextOutput
        ""
    )
    ( Comm.KeyInput "Press the 's' key to start!" case _ of
        { name: "s" } -> Just Start
        _ -> Nothing
    )

  CountingDown n -> Comm.Surface
    (Comm.TextOutput $ show n <> "...")
    (Comm.NoInput)

  LaunchSpaceShip -> Comm.Surface
    (Comm.TextOutput $ "boom!")
    (Comm.NoInput)

initialState :: State
initialState = Init

program :: Mar.Program Msg State
program =
  { initialState
  , renderer: Comm.mkRenderer_ view
  , controller: RecAPI.mkController control
  }

main :: Effect Unit
main = launchAff_ do
  _ <- Mar.runProgram program Mar.defaultConfig
  pure unit
