-- | The main module of the `marionette` runtime. You need a `Controller` and a
-- | `Renderer` in addition to the types and functions that are defined in here to
-- | run a state machine program.
module Marionette
  ( Config
  , EventType(..)
  , Program
  , ProgramEvent(..)
  , ThreadId(..)
  , defaultConfig
  , noController
  , noRenderer
  , runProgram
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Error, Fiber, error, launchAff, launchAff_, makeAff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Marionette.Types (Controller(..), Renderer(..), State(..))

-- | Type variables:
-- | - `msg` the message type of your program
-- | - `sta` the state type of your program
-- |
-- | Record fields:
-- | - `initialState` 
-- | - `renderer` specifies how state is rendered
-- | - `controller` specifies the how the control flow is handled
type Program msg sta =
  { initialState :: sta
  , renderer :: Renderer msg sta
  , controller :: Controller msg sta
  }

-- | Type variables:
-- | - `msg` the message type of your program
-- | - `sta` the state type of your program
-- |
-- | Record fields:
-- | - `exitIf` provides an exit condition to leave the running state machine
-- |   based on the current message/state combination.
-- | - `initialMsg` useful if the first message event is not triggered by user input.
-- | - `onEvent` lets you observe events that occur inside the `marionette`
-- |   runtime. This is useful for either debugging or other kind of state
-- |   machine flow analysis. 
type Config msg sta =
  { exitIf :: msg -> sta -> Boolean
  , initialMsg :: Maybe msg
  , onEvent :: ProgramEvent msg sta -> Effect Unit
  }

-- | Every time a message is raised a new thread with an unique `ThreadId` is created that has read and
-- | write access to the program's state.
newtype ThreadId = ThreadId Int

-- | General Program event type that contains a timestamp and a more specific
-- | event type.
data ProgramEvent msg sta = ProgramEvent Instant (EventType msg sta)

-- | Possible observable events that may occur inside the `marionette` runtime. 
data EventType msg sta
  -- | new thread is triggered by a message event
  = NewMsg ThreadId msg
  -- | specific thread triggered by a message event has finished running
  | EndMsg ThreadId
  -- | new state update has been performed and a re-rendering was triggered
  | NewState ThreadId sta

type Env msg sta =
  { stateRef :: Ref sta
  , nextThreadIdRef :: Ref ThreadId
  , threadsRef :: Ref (Map ThreadId (Fiber Unit))
  , programCallback :: Either Error sta -> Effect Unit
  , program :: Program msg sta
  , config :: Config msg sta
  }

---

derive newtype instance Enum ThreadId

derive newtype instance Bounded ThreadId

derive newtype instance Ord ThreadId

derive newtype instance Eq ThreadId

derive newtype instance Semiring ThreadId

derive instance Generic ThreadId _

derive instance Generic (ProgramEvent msg sta) _

derive instance Generic (EventType msg sta) _

instance Show ThreadId where
  show = genericShow

instance (Show msg, Show sta) => Show (ProgramEvent msg sta) where
  show = genericShow

instance (Show msg, Show sta) => Show (EventType msg sta) where
  show = genericShow

---

-- | A renderer that does nothing. Useful if like to run a headless state machine.
noRenderer :: forall msg sta. Renderer msg sta
noRenderer = Renderer
  { onInit: pure unit
  , onState: \_ _ -> pure unit
  , onFinish: pure unit
  }

-- | A controller that does nothing. Useful if you just want to render something and the controller is not implemented yet.
noController :: forall msg sta. Controller msg sta
noController = Controller \_ _ _ -> pure unit

-- | Some defaults for the [Config](#t:Config) type
defaultConfig :: forall msg sta. Config msg sta
defaultConfig =
  { initialMsg: Nothing
  , exitIf: neverExit
  , onEvent: \_ -> pure unit
  }

-- | Takes a stateful program specification and some configuration and runs the
-- | program in an `Aff` context.
-- |
-- | Once the program has finished the final state is returned and all running
-- | threads are aborted.
runProgram :: forall msg sta. Eq sta => Program msg sta -> Config msg sta -> Aff sta
runProgram program config = makeAff \programCallback -> do
  stateRef <- Ref.new program.initialState
  threadsRef <- Ref.new Map.empty
  nextThreadIdRef <- Ref.new (ThreadId 0)

  let
    env =
      { stateRef
      , threadsRef
      , nextThreadIdRef
      , program
      , config
      , programCallback
      }

  launchAff_ do
    initProgram env
    view env

  pure (Canceler \_ -> cleanup env)

---

neverExit :: forall msg sta. msg -> sta -> Boolean
neverExit _ _ = false

mkProgramEvent :: forall msg sta. EventType msg sta -> Effect (ProgramEvent msg sta)
mkProgramEvent ev = do
  instant <- now
  pure $ ProgramEvent instant ev

cleanup :: forall msg sta. Env msg sta -> Aff Unit
cleanup { stateRef, threadsRef, programCallback, program } = do
  state <- liftEffect $ Ref.read stateRef
  threads <- liftEffect $ Ref.read threadsRef
  threads
    # Map.values
    # traverse_ (Aff.killFiber $ error "Cleanup error")
  (unwrap program.renderer).onFinish

initProgram :: forall msg sta. Eq sta => Env msg sta -> Aff Unit
initProgram env = do
  (unwrap env.program.renderer).onInit
  case env.config.initialMsg of
    Just msg -> runFreshThread env msg
    Nothing -> pure unit

runFreshThread :: forall msg sta. Eq sta => Env msg sta -> msg -> Aff Unit
runFreshThread env msg = checkExit env msg do

  id <- liftEffect $
    Ref.read env.nextThreadIdRef <*
      Ref.modify_ (_ + one) env.nextThreadIdRef

  liftEffect $ env.config.onEvent =<< mkProgramEvent (NewMsg id msg)

  fiber <- liftEffect $ launchAff $ (unwrap env.program.controller) (runFreshThread env) (mkStateImpl env id) msg
  liftEffect $ Ref.modify_ (Map.insert id fiber) env.threadsRef
  Aff.joinFiber fiber

  liftEffect $ Ref.modify_ (Map.delete id) env.threadsRef

  liftEffect $ env.config.onEvent =<< mkProgramEvent (EndMsg id)

  pure unit

mkStateImpl :: forall msg sta. Eq sta => Env msg sta -> ThreadId -> State sta Aff
mkStateImpl env id = State \f -> do
  s <- liftEffect $ Ref.read env.stateRef
  let Tuple x s' = f s

  pure x <* when (s /= s') do
    liftEffect $ env.config.onEvent =<< mkProgramEvent (NewState id s')
    liftEffect $ Ref.write s' env.stateRef
    view env

view :: forall msg sta. Eq sta => Env msg sta -> Aff Unit
view env = do
  state <- liftEffect $ Ref.read env.stateRef
  (unwrap env.program.renderer).onState state (runFreshThread env)

checkExit :: forall msg sta. Env msg sta -> msg -> Aff Unit -> Aff Unit
checkExit env msg cont = do
  state <- liftEffect $ Ref.read env.stateRef
  if env.config.exitIf msg state then do
    threads <- liftEffect $ Ref.read env.threadsRef
    threads
      # Map.values
      # traverse_ (Aff.killFiber $ error "Cleanup error")
    (unwrap env.program.renderer).onFinish
    liftEffect $ env.programCallback $ Right state
  else
    cont
