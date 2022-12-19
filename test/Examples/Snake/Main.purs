module Test.Examples.Snake.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Random (randomInt)
import Marionett.Controllers.Monadic as Monadic
import Marionette (Config, Program, defaultConfig)
import Marionette as Mar
import Marionette.Renderers.Commander as Commander
import Test.Examples.Snake.MVC.Control (Env, control)
import Test.Examples.Snake.MVC.Model (Msg, State(..))
import Test.Examples.Snake.MVC.View (view)

env :: Env Aff
env =
  { delay: Aff.delay
  , randomInt: liftEffect $ randomInt bottom top
  }

config :: Config Msg State
config =
  defaultConfig

program :: Program Msg State
program =
  { initialState: State_Init
  , renderer: Commander.mkRenderer view $ Commander.defaultConfig
      { clearScreen = true
      }
  , controller: Monadic.mkController $ control env
  }

main :: Effect Unit
main = launchAff_ do
  res <- Mar.runProgram program config
  logShow res
  pure unit
