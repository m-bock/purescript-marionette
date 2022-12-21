module Test.Examples.PureCounter where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Marionette.ControlHandlers.Pure as PureCtrl
import Marionette as Mar
import Marionette.Renderers.Commander as Comm

type State = Int

data Msg = CountUp | CountDown

update :: Msg -> State -> State
update msg state = case msg of
  CountUp -> state + 1
  CountDown -> state - 1

view :: State -> Comm.CliSurface Msg
view count = Comm.CliSurface
  ( Comm.TextOutput $
      "Current count: " <> show count
  )
  ( Comm.KeyInput "Use up/down keys" case _ of
      { name: "up" } -> Just CountUp
      { name: "down" } -> Just CountDown
      _ -> Nothing
  )

initialState :: State
initialState = 0

program :: Mar.Program Msg State
program =
  { initialState
  , renderer: Comm.mkRenderer_ view
  , controller: PureCtrl.mkController update
  }

main :: Effect Unit
main = launchAff_ do
  _ <- Mar.runProgram program Mar.defaultConfig
  pure unit
