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

--------------------------------------------------------------------------------
--- Public
--------------------------------------------------------------------------------

-- | Type variables:
-- | - `msg` message type of your program
-- | - `sta` state type of your program
-- |
-- | Record fields:
-- | - `initialState` value of the initial state
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
-- | - `stateHandler` The state implementation that is used by the runtime. 
type Config msg sta =
  { exitIf :: msg -> sta -> Boolean
  , initialMsg :: Maybe msg
  , onEvent :: ProgramEvent msg sta -> Effect Unit
  , stateHandler :: sta -> Effect (State sta Aff)
  }

-- | Every time a message is raised a new thread with an unique `ThreadId` is created that has read and
-- | write access to the program's state.
newtype ThreadId = ThreadId Int

-- | General program event type that contains a timestamp and a more specific
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
  { stateHandler :: State sta Aff
  , nextThreadIdRef :: Ref ThreadId
  , threadsRef :: Ref (Map ThreadId (Fiber Unit))
  , programCallback :: Either Error sta -> Effect Unit
  , program :: Program msg sta
  , config :: Config msg sta
  }

-- | A controller that does nothing. Useful if you just want to render something and the controller is not implemented yet.
noController :: forall msg sta. Controller msg sta
noController = Controller \_ _ _ -> pure unit

-- | Some defaults for the [Config](#t:Config) type
defaultConfig :: forall msg sta. Eq sta => Config msg sta
defaultConfig =
  { initialMsg: Nothing
  , exitIf: neverExit
  , onEvent: \_ -> pure unit
  , stateHandler: stateHandlerRef
  }

stateHandlerRef :: forall s. Eq s => s -> Effect (State s Aff)
stateHandlerRef initialState = do
  stateRef <- Ref.new initialState

  pure $ State \f -> liftEffect $ do
    s <- Ref.read stateRef
    let Tuple x s' = f s
    pure x <* when (s /= s') do
      Ref.write s' stateRef

-- | Takes a stateful program specification and some configuration and runs the
-- | program in an `Aff` context.
-- |
-- | Once the program has finished the final state is returned and all running
-- | threads are aborted.
runProgram :: forall msg sta. Eq sta => Program msg sta -> Config msg sta -> Aff sta
runProgram program config = makeAff \programCallback -> do
  stateHandler <- config.stateHandler program.initialState
  threadsRef <- Ref.new Map.empty
  nextThreadIdRef <- Ref.new (ThreadId 0)

  let
    env =
      { stateHandler
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

-- | A renderer that does nothing. Useful if like to run a headless state machine.
noRenderer :: forall msg sta. Renderer msg sta
noRenderer = Renderer
  { onInit: \_ -> pure unit
  , onState: \_ _ -> pure unit
  , onFinish: pure unit
  }

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
--- Core
--------------------------------------------------------------------------------

neverExit :: forall msg sta. msg -> sta -> Boolean
neverExit _ _ = false

mkProgramEvent :: forall msg sta. EventType msg sta -> Effect (ProgramEvent msg sta)
mkProgramEvent ev = do
  instant <- now
  pure $ ProgramEvent instant ev

cleanup :: forall msg sta. Env msg sta -> Aff Unit
cleanup { stateHandler: State state, threadsRef, programCallback, program } = do
  state <- state \s -> Tuple s s
  threads <- liftEffect $ Ref.read threadsRef
  threads
    # Map.values
    # traverse_ (Aff.killFiber $ error "Cleanup error")
  (unwrap program.renderer).onFinish

initProgram :: forall msg sta. Eq sta => Env msg sta -> Aff Unit
initProgram env = do
  (unwrap env.program.renderer).onInit (runFreshThread env)
  case env.config.initialMsg of
    Just msg -> runFreshThread env msg
    Nothing -> pure unit

runFreshThread :: forall msg sta. Eq sta => Env msg sta -> msg -> Aff Unit
runFreshThread env msg = checkExit env msg do

  id <- liftEffect $
    Ref.read env.nextThreadIdRef <*
      Ref.modify_ (_ + one) env.nextThreadIdRef

  liftEffect $ env.config.onEvent =<< mkProgramEvent (NewMsg id msg)

  fiber <- liftEffect $ launchAff $
    (unwrap env.program.controller)
      (runFreshThread env)
      (hookStateHandler env.stateHandler env id)
      msg
  liftEffect $ Ref.modify_ (Map.insert id fiber) env.threadsRef
  Aff.joinFiber fiber

  liftEffect $ Ref.modify_ (Map.delete id) env.threadsRef

  liftEffect $ env.config.onEvent =<< mkProgramEvent (EndMsg id)

  pure unit

hookStateHandler :: forall msg sta. Eq sta => State sta Aff -> Env msg sta -> ThreadId -> State sta Aff
hookStateHandler (State st1) env id = State \f -> do
  s <- st1 get
  let Tuple x s' = f s

  pure x <* when (s /= s') do
    liftEffect $ env.config.onEvent =<< mkProgramEvent (NewState id s')
    st1 $ set s'
    view env
  where
  get s = Tuple s s
  set s _ = Tuple unit s

view :: forall msg sta. Eq sta => Env msg sta -> Aff Unit
view env@{ stateHandler: State state } = do
  state' <- state \s -> Tuple s s
  (unwrap env.program.renderer).onState state' (runFreshThread env)

checkExit :: forall msg sta. Env msg sta -> msg -> Aff Unit -> Aff Unit
checkExit env@{ stateHandler: State state } msg cont = do
  state' <- state \s -> Tuple s s
  if env.config.exitIf msg state' then do
    threads <- liftEffect $ Ref.read env.threadsRef
    threads
      # Map.values
      # traverse_ (Aff.killFiber $ error "Cleanup error")
    (unwrap env.program.renderer).onFinish
    liftEffect $ env.programCallback $ Right state'
  else
    cont
