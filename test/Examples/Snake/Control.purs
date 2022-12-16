module Test.Examples.Snake.Control where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadReader, ReaderT, runReaderT)
import Control.Monad.State (class MonadState, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), fromRight, note)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect.Aff (Aff)
import Marionett.Controllers.Monadic (MarionetteT, sendMsg)
import Test.Examples.Snake.Core (Board(..), Goodie(..), LevelSpec(..), Maze(..), Snake(..), findFreeSpots)
import Test.Examples.Snake.Core as Core
import Test.Examples.Snake.Model (Game(..), Msg(..), Score(..), State(..))
import Unsafe.Coerce (unsafeCoerce)

type Env m =
  { delay :: Milliseconds -> m Unit
  , randomInt :: Int -> Int -> m Int
  }

eitherLevelSpec :: Either String LevelSpec
eitherLevelSpec = Core.parseLevelSpec $ Str.joinWith "\n"
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

data AppError = Err1 | Err2

-- class
--   ( MonadState State m
--   , MonadReader (Env m) m
--   , MonadError AppError m
--   , MonadSendMsg Msg m
--   ) <=
--   MonadApp m

type T m a = ReaderT (Env m) (MarionetteT Msg State m) a

--runT :: forall a. Env Aff -> T a -> (MarionetteT Msg State ( Aff) a)
-- runT env ma = ma
--   # flip runReaderT env
--   # runExceptT
--   <#> fromRight noCon

-- control :: forall m. MonadApp m => Msg -> m Unit
-- control = unsafeCoerce

control :: forall m. Monad m => Env m -> Msg -> MarionetteT Msg State m Unit
control env = case _ of
  Msg_Start -> do
    eitherGame <- lift $ runExceptT do
      levelSpec <- ExceptT $ pure $ eitherLevelSpec
      initGame env levelSpec

    modify_ case _ of
      Sta_Init -> case eitherGame of
        Right game -> Sta_Playing game
        Left msg -> Sta_Error msg
      st -> st

    sendMsg Msg_Tick
  _ -> pure unit

initGame :: forall m. Monad m => Env m -> LevelSpec -> ExceptT String m Game
initGame env (LevelSpec snake maze direction) = ado
  goodie <- newGoodie env snake maze
  in
    Game
      { snake
      , maze
      , goodie
      , score: Score 0
      , direction
      }

pickRandomly :: forall m a. Monad m => Env m -> Array a -> ExceptT String m a
pickRandomly env xs = do
  index <- lift $ env.randomInt 0 (Arr.length xs)
  ExceptT $ pure $ note "rand" $  Arr.index xs index

newGoodie :: forall m. Monad m => Env m -> Snake -> Maze -> ExceptT String m Goodie
newGoodie env snake maze = do
  board <- Core.mazeToBoard maze # Core.applySnake snake # note "mazeToboard" # pure >>> ExceptT
  let spots = findFreeSpots board
  vec <- pickRandomly env spots
  pure $ Goodie vec

-- control :: forall m. Monad m => Env m -> Msg -> MarionetteT Msg State m Unit
-- control env = case _ of
--   Msg_Start -> do
--     modify_ case _ of
--       Sta_Init -> Sta_Playing initGame
--       Sta_Pause game -> Sta_Playing game
--       st -> st
--     sendMsg Msg_Tick

--   Msg_Tick -> do
--     state <- get
--     case state of
--       Sta_Playing game -> do
--         nextState <- lift $ next env game
--         put nextState
--       st -> pure unit
--     sendMsg Msg_Tick

--   Msg_Navigate dir ->
--     modify_ case _ of
--       Sta_Playing game -> Sta_Playing game { direction = dir }
--       st -> st

--   Msg_Pause -> modify_ case _ of
--     Sta_Playing game -> Sta_Pause game
--     st -> st
