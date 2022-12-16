module Marionett.Controllers.Monadic
  ( MarionetteT
  , mkController
  , sendMsg
  ) where

import Prelude

import Control.Monad.Reader (class MonadTrans, ReaderT(..), ask, lift, runReaderT)
import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Marionette.Types (Controller(..), SendMsg, State(..))

type Control msg sta m = msg -> MarionetteT msg sta m Unit

---

newtype MarionetteT msg sta (m :: Type -> Type) a = MarionetteT (ReaderT (MarionetteEnv msg sta m) m a)

newtype MarionetteEnv msg sta (m :: Type -> Type) = MarionetteEnv { state :: State sta m, sendMsg :: SendMsg msg m }

derive newtype instance Monad m => Monad (MarionetteT msg sta m)

derive newtype instance Bind m => Bind (MarionetteT msg sta m)

derive newtype instance Apply m => Apply (MarionetteT msg sta m)

derive newtype instance Applicative m => Applicative (MarionetteT msg sta m)

derive newtype instance Functor m => Functor (MarionetteT msg sta m)

instance MonadTrans (MarionetteT msg sta)
  where
  lift = MarionetteT <<< lift

instance Monad m => MonadState sta (MarionetteT msg sta m) where
  state f = MarionetteT do
    MarionetteEnv { state: State state } <- ask
    lift $ state f

runMarionetteT :: forall msg sta m a. SendMsg msg m -> State sta m -> MarionetteT msg sta m a -> m a
runMarionetteT sendMsg_ state (MarionetteT ma) = runReaderT ma $ MarionetteEnv { sendMsg: sendMsg_, state }

sendMsg :: forall msg sta m. Monad m => msg -> MarionetteT msg sta m Unit
sendMsg msg = MarionetteT do
  MarionetteEnv env <- ask
  lift $ env.sendMsg msg

mkController :: forall msg sta. Control msg sta Aff -> Controller msg sta
mkController mc = Controller \sendMsg_ state msg -> runMarionetteT sendMsg_ state $ mc msg

-- class MonadSendMsg msg m where
--   sendMsg :: msg -> m Unit

-- class
--   ( MonadSendMsg msg m
--   , MonadState sta m
--   ) <=
--   MonadMarionette msg sta m