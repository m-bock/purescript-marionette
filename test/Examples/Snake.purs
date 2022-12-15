module Test.Examples.Snake where

import Prelude

import Data.Maybe (Maybe, fromJust, maybe')
import Data.String as Str
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Marionette as Mar
import Partial.Unsafe (unsafeCrashWith)
import Test.Examples.Snake.Core (LevelSpec(..), Maze(..), Snake(..))
import Test.Examples.Snake.Core as Core
import Test.Examples.Snake.Direction (Direction)
import Test.Examples.Snake.Vector (Vector)
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

newtype Game = Game
  { snake :: Snake
  , maze :: Maze
  , score :: Int
  , goodie :: Vector Int
  , direction :: Direction
  }

type Env m =
  { delay :: Milliseconds -> m Unit
  --, random :: m Int
  }

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


initGame :: forall m. Monad m => Env m -> LevelSpec -> m Game
initGame env (LevelSpec snake maze direction) = ado 
  in
    Game
      { snake
      , maze
      , goodie: unsafeCoerce 1
      , score: 0
      , direction
      }


main :: Effect Unit
main = launchAff_ do
  --let levelSpec = maybe' (\_ -> unsafeCrashWith "wrong config") level
  --game <- initGame env levelSpec

  _ <- Mar.runProgram Sta_Init Mar.defaultProgramConfig
  pure unit

  where 
    env = {
      delay: Aff.delay
    }