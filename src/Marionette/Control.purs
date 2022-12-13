module Marionett.Control
  ( MarionetteT
  , RecordAPI
  , monadicControl
  , pureControl
  , recordApiControl
  , sendMsg
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Marionette.Types (Control, State, SendMsg)

type PureControl msg sta = msg -> sta -> sta

type MonadicControl msg sta m = msg -> MarionetteT msg sta m (Maybe msg)

type RecordApiControl msg sta = RecordAPI msg sta -> msg -> Aff (Maybe msg)

---

pureControl :: forall msg sta. PureControl msg sta -> Control msg sta
pureControl update _ state msg = do
  modifyState_ $ update msg
  pure Nothing
  where
  modifyState_ = \f -> state \s -> Tuple unit (f s)

---

newtype MarionetteT msg sta m a = MarionetteT (ReaderT (MarionetteEnv msg sta m) m a)

newtype MarionetteEnv msg sta m = MarionetteEnv { state :: State sta m, sendMsg :: SendMsg msg m }

derive newtype instance Monad m => Monad (MarionetteT msg sta m)
derive newtype instance Bind m => Bind (MarionetteT msg sta m)
derive newtype instance Apply m => Apply (MarionetteT msg sta m)
derive newtype instance Applicative m => Applicative (MarionetteT msg sta m)
derive newtype instance Functor m => Functor (MarionetteT msg sta m)

instance Monad m => MonadState sta (MarionetteT msg sta m) where
  state f = MarionetteT do
    MarionetteEnv env <- ask
    lift $ env.state f

runMarionetteT :: forall msg sta m a. SendMsg msg m -> State sta m -> MarionetteT msg sta m a -> m a
runMarionetteT sendMsg_ state (MarionetteT ma) = runReaderT ma $ MarionetteEnv { sendMsg: sendMsg_, state }

sendMsg :: forall msg sta m. Monad m => msg -> MarionetteT msg sta m Unit
sendMsg msg = MarionetteT do
  MarionetteEnv env <- ask
  lift $ env.sendMsg msg

monadicControl :: forall msg sta. MonadicControl msg sta Aff -> Control msg sta
monadicControl mc sendMsg_ state msg = runMarionetteT sendMsg_ state $ mc msg

---

type RecordAPI msg sta =
  { sendMsg :: msg -> Aff Unit
  , getState :: Aff sta
  , putState :: sta -> Aff Unit
  , modifyState :: (sta -> sta) -> Aff sta
  , modifyState_ :: (sta -> sta) -> Aff Unit
  }

recordApiControl :: forall msg sta. RecordApiControl msg sta -> Control msg sta
recordApiControl control sendMsg_ state msg =
  control api msg
  where
  api =
    { sendMsg: sendMsg_
    , getState: state \s -> Tuple s s
    , putState: \s -> state \_ -> Tuple unit s
    , modifyState: \f -> state \s -> let s' = f s in Tuple s' s'
    , modifyState_: \f -> state \s -> Tuple unit (f s)
    }