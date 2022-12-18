module Test.Examples.Snake.Main where

import Prelude

import Data.Maybe (Maybe, fromJust, maybe')
import Data.String as Str
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Random (randomInt)
import Marionett.Controllers.Monadic as Monadic
import Marionette (Config, Program, defaultConfig, noController, noRenderer)
import Marionette as Mar
import Marionette.Renderers.Commander as Commander
import Marionette.Renderers.Eventless as Eventless
import Partial.Unsafe (unsafeCrashWith)
import Test.Examples.Snake.MVC.Control (Env, control)
import Test.Examples.Snake.Core (LevelSpec(..), Maze(..), Snake(..))
import Test.Examples.Snake.Core as Core
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Vector (Vector)
import Test.Examples.Snake.MVC.Model (Msg, State(..))
import Test.Examples.Snake.MVC.View (view)
import Unsafe.Coerce (unsafeCoerce)

-- initGame :: forall m. Monad m => Env m -> LevelSpec -> m Game
-- initGame env (LevelSpec snake maze direction) = ado 
--   in
--     Game
--       { snake
--       , maze
--       , goodie: unsafeCoerce 1
--       , score: 0
--       , direction
--       }

env :: Env Aff
env =
  { delay: Aff.delay
  , randomInt: liftEffect $ randomInt 0 1000 -- bottom top
  }

config :: Config Msg State
config =
  defaultConfig

program :: Program Msg State
program =
  { initialState: Sta_Init
  , renderer: Commander.mkRenderer view $ Commander.defaultConfig {
    clearScreen = true
  }
  , controller: Monadic.mkController $ control env
  }

-- { renderer = Eventless.mkRenderer_ view
-- --, controller = F.mkController_ control
-- }

main :: Effect Unit
main = launchAff_ do
  --let levelSpec = maybe' (\_ -> unsafeCrashWith "wrong config") level
  --game <- initGame env levelSpec
  log "xx"

  res <- Mar.runProgram program config
  log "x"
  logShow res
  pure unit
