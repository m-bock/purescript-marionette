module Marionett.ControlHandlers.Monadic
  ( MarionetteT
  , monadicControlHandler
  , sendMsg
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Marionette.Types (ControlHandler, State, SendMsg)

type MonadicControlHandler msg sta m = msg -> MarionetteT msg sta m (Maybe msg)

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

monadicControlHandler :: forall msg sta. MonadicControlHandler msg sta Aff -> ControlHandler msg sta
monadicControlHandler mc sendMsg_ state msg = runMarionetteT sendMsg_ state $ mc msg

