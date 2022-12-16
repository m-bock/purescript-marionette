module Test.Examples.Snake.Main where

import Prelude

import Data.Maybe (Maybe, fromJust, maybe')
import Data.String as Str
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Marionette (Program, defaultProgram)
import Marionette as Mar
import Marionette.Renderers.Eventless as Eventless
import Partial.Unsafe (unsafeCrashWith)
import Test.Examples.Snake.Core (LevelSpec(..), Maze(..), Snake(..))
import Test.Examples.Snake.Core as Core
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Vector (Vector)
import Test.Examples.Snake.Model (State(..))
import Test.Examples.Snake.View (view)
import Unsafe.Coerce (unsafeCoerce)

level :: Maybe LevelSpec
level = Core.parseLevelSpec $ Str.joinWith "\n"
  [ "############    ####"
  , "#                  #"
  , "     OOOO+         #"
  , "                   #"
  , "                   #"
  , "#                  #"
  , "#                   "
  , "#                   "
  , "#                  #"
  , "####      ##########"
  ]

type Env m =
  { delay :: Milliseconds -> m Unit
  --, random :: m Int
  }

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

program :: Program Unit State
program = defaultProgram
  { initialState = Sta_Init
  , renderer = Eventless.mkRenderer_ view
  --, controller = F.mkController_ control
  }

main :: Effect Unit
main = launchAff_ do
  --let levelSpec = maybe' (\_ -> unsafeCrashWith "wrong config") level
  --game <- initGame env levelSpec

  _ <- Mar.runProgram program
  pure unit

  where
  env =
    { delay: Aff.delay
    }