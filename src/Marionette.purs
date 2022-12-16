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
  )
  where

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

type Program msg sta =
  { initialState :: sta
  , renderer :: Renderer msg sta
  , controller :: Controller msg sta
  }

type Config msg sta =
  { exitIf :: msg -> sta -> Boolean
  , initialMsg :: Maybe msg
  , onEvent :: ProgramEvent msg sta -> Effect Unit
  }

newtype ThreadId = ThreadId Int

derive newtype instance Enum ThreadId
derive newtype instance Bounded ThreadId
derive newtype instance Ord ThreadId
derive newtype instance Eq ThreadId
derive newtype instance Semiring ThreadId

derive instance Generic ThreadId _

instance Show ThreadId where
  show = genericShow

noRenderer :: forall msg sta. Renderer msg sta
noRenderer = Renderer
  { onInit: pure unit
  , onState: \_ _ -> pure unit
  , onFinish: pure unit
  }

noController :: forall msg sta. Controller msg sta
noController = Controller \_ _ _ -> pure unit

defaultConfig :: forall msg sta. Config msg sta
defaultConfig =
  { initialMsg: Nothing
  , exitIf: neverExit
  , onEvent: \_ -> pure unit
  }

neverExit :: forall msg sta. msg -> sta -> Boolean
neverExit _ _ = false

type Env msg sta =
  { stateRef :: Ref sta
  , nextThreadIdRef :: Ref ThreadId
  , threadsRef :: Ref (Map ThreadId (Fiber Unit))
  , programCallback :: Either Error sta -> Effect Unit
  , program :: Program msg sta
  , config :: Config msg sta
  }

data ProgramEvent msg sta = ProgramEvent Instant (EventType msg sta)

derive instance Generic (ProgramEvent msg sta) _

instance (Show msg, Show sta) => Show (ProgramEvent msg sta) where
  show = genericShow

derive instance Generic (EventType msg sta) _

instance (Show msg, Show sta) => Show (EventType msg sta) where
  show = genericShow

data EventType msg sta
  = NewMsg ThreadId msg
  | EndMsg ThreadId
  | NewState ThreadId sta

type StateRef_ sta r = (stateRef :: Ref sta | r)

type ThreadsRef_ msg r = (threadsRef :: Ref (Map ThreadId (Fiber (Maybe msg))) | r)

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
  when (env.config.exitIf msg state) do
    threads <- liftEffect $ Ref.read env.threadsRef
    threads
      # Map.values
      # traverse_ (Aff.killFiber $ error "Cleanup error")
    liftEffect $ env.programCallback $ Right state
    (unwrap env.program.renderer).onFinish
    cont

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
