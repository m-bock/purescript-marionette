-- | This module provides the most flexible way to handle state control via a
-- | `MarionetteT` monad transformer.

module Marionette.Controllers.Monadic
  ( Control
  , MarionetteT
  , mkController
  , sendMsg
  ) where

import Prelude

import Control.Monad.Reader (class MonadTrans, ReaderT, ask, lift, runReaderT)
import Control.Monad.State (class MonadState)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Marionette.Types (Controller(..), SendMsg, State(..))

--------------------------------------------------------------------------------
--- Public
--------------------------------------------------------------------------------

-- | A monad transformer that adds the following features to the base monad `m`
-- |
-- | - read/write access to the program's state (`sta`) via a StateMonad instance
-- | - the ability to use `sendMsg` to raise messages (values of type `msg`)
-- |   from within a control handler

newtype MarionetteT msg sta (m :: Type -> Type) a =
  MarionetteT (ReaderT (MarionetteEnv msg sta m) m a)

derive newtype instance Monad m => Monad (MarionetteT msg sta m)

derive newtype instance Bind m => Bind (MarionetteT msg sta m)

derive newtype instance Apply m => Apply (MarionetteT msg sta m)

derive newtype instance Applicative m => Applicative (MarionetteT msg sta m)

derive newtype instance Functor m => Functor (MarionetteT msg sta m)

derive newtype instance MonadEffect m => MonadEffect (MarionetteT msg sta m)

derive newtype instance MonadAff m => MonadAff (MarionetteT msg sta m)

instance MonadTrans (MarionetteT msg sta)
  where
  lift = MarionetteT <<< lift

instance Monad m => MonadState sta (MarionetteT msg sta m) where
  state f = MarionetteT do
    MarionetteEnv { state: State state } <- ask
    lift $ state f

-- | Triggers a new message from within a control handler
sendMsg :: forall msg sta m. Monad m => msg -> MarionetteT msg sta m Unit
sendMsg msg = MarionetteT do
  MarionetteEnv env <- ask
  lift $ env.sendMsg msg

-- | Creates a low level controller when given a high level control function
mkController :: forall msg sta. Control msg sta Aff -> Controller msg sta
mkController mc = Controller \sendMsg_ state msg -> runMarionetteT sendMsg_ state $ mc msg

-- | The high level control function. Takes a `msg` and returns a computation
-- | inside the `MarionetteT` monad transformer. 
type Control msg sta m = msg -> MarionetteT msg sta m Unit

--------------------------------------------------------------------------------
--- Util
--------------------------------------------------------------------------------

newtype MarionetteEnv msg sta (m :: Type -> Type) =
  MarionetteEnv { state :: State sta m, sendMsg :: SendMsg msg m }

runMarionetteT :: forall msg sta m a. SendMsg msg m -> State sta m -> MarionetteT msg sta m a -> m a
runMarionetteT sendMsg_ state (MarionetteT ma) =
  runReaderT ma $ MarionetteEnv { sendMsg: sendMsg_, state }
