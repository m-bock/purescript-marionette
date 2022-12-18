module Test.Examples.Snake.MVC.Control where

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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect.Aff (Aff)
import Marionett.Controllers.Monadic (MarionetteT, sendMsg)
import Test.Examples.Snake.Core (Goodie(..), LevelSpec(..), Maze(..), Snake(..), findFreeSpots)
import Test.Examples.Snake.Core as Core
import Test.Examples.Snake.Data.Board (Board, ParseError)
import Test.Examples.Snake.Data.Board as Board
import Test.Examples.Snake.MVC.Model (Game(..), Msg(..), Score(..), State(..), StateError(..))
import Unsafe.Coerce (unsafeCoerce)

type Env m =
  { delay :: Milliseconds -> m Unit
  , randomInt :: m Int
  }

-- eitherBoard :: Either ParseError Board
-- eitherBoard = Board.parse $ Str.joinWith "\n"
--   [ "############    ####"
--   , "#                  #"
--   , "   +OO             #"
--   , "                   #"
--   , "          x        #"
--   , "#                  #"
--   , "#                   "
--   , "#                   "
--   , "#                  #"
--   , "####      ##########"
--   ]

eitherBoard :: Either ParseError Board
eitherBoard = Board.parse $ Str.joinWith "\n"
  [ "############    ####"
  , "#                  #"
  , "  +OOOO            #"
  , "                   #"
  , "    x              #"
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
    modify_ case _ of
      Sta_Init -> case eitherBoard of
        Right board -> Sta_Playing $ Game { board, score: Score 0 }
        Left msg -> Sta_Error $ ErrBoardParse $ show msg
      st -> st

    sendMsg Msg_Tick

  Msg_Resume -> do
    modify_ case _ of
      Sta_Paused game -> Sta_Playing game
      st -> st

  Msg_Pause -> do
    modify_ case _ of
      Sta_Playing game -> Sta_Paused game
      st -> st

  Msg_Navigate dir -> do
    modify_ case _ of
      Sta_Playing (Game game) -> Sta_Playing $ Game $ game
        { board = Board.setDirection dir game.board # fromMaybe game.board
        }
      st -> st

  Msg_Tick -> do
    randInt <- lift $ Board.RandInt <$> env.randomInt
    modify_ case _ of
      Sta_Playing (Game game) ->
        case Board.next randInt game.board of
          Just newBoard ->
            let
              newScore = game.score + Score 1
            in
              if newScore >= Score 10 then
                Sta_Won newScore
              else
                Sta_Playing $ Game game { board = newBoard, score = newScore }
          Nothing -> Sta_Lost game.score
      st -> st

    lift $ env.delay (Milliseconds 1000.0)

    sendMsg Msg_Tick


-- state <- get
-- case state of
--   Sta_Playing game -> do
--     nextState <- lift $ next env game
--     put nextState
--   st -> pure unit
-- sendMsg Msg_Tick

-- initGame :: forall m. Monad m => Env m -> LevelSpec -> ExceptT String m Game
-- initGame env (LevelSpec snake maze direction) = ado
--   goodie <- newGoodie env snake maze
--   in
--     Game
--       { snake
--       , maze
--       , goodie
--       , score: Score 0
--       , direction
--       }

-- pickRandomly :: forall m a. Monad m => Env m -> Array a -> ExceptT String m a
-- pickRandomly env xs = do
--   index <- lift $ env.randomInt 0 (Arr.length xs)
--   ExceptT $ pure $ note "rand" $ Arr.index xs index

-- newGoodie :: forall m. Monad m => Env m -> Snake -> Maze -> ExceptT String m Goodie
-- newGoodie env snake maze = do
--   board <- Core.mazeToBoard maze # Core.applySnake snake # note "mazeToboard" # pure >>> ExceptT
--   let spots = findFreeSpots board
--   vec <- pickRandomly env spots
--   pure $ Goodie vec

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
