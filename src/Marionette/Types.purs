-- | This module contains types that are only needed for low level
-- | Renderer/Controller implementations.

module Marionette.Types
  ( Controller(..)
  , Renderer(..)
  , SendMsg
  , State(..)
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)

--------------------------------------------------------------------------------
--- Public
--------------------------------------------------------------------------------

-- | Low level state implementation that allows to derive reading and updating
-- | state functions
newtype State s m = State (forall a. (s -> Tuple a s) -> m a)

-- | Defines how messages can be triggered by the runtime
type SendMsg msg m = msg -> m Unit

-- | A controller is defined as a computation that runs in `Aff` when given 
-- | an implementation to send messages, a low level `State` implementation
-- | along with the current a messsage that has been triggered.
newtype Controller msg sta = Controller
  (SendMsg msg Aff -> State sta Aff -> msg -> Aff Unit)

-- | Type variables:
-- | - `msg` message type of your program
-- | - `sta` state type of your program
-- |
-- | Record fields:
-- | - `onInit` runs before the state machine starts
-- | - `onState` runs on every state update
-- | - `onFinish` runs before the state machine exits
newtype Renderer msg sta = Renderer
  { onInit :: (msg -> Aff Unit) -> Aff Unit
  , onState :: sta -> (msg -> Aff Unit) -> Aff Unit
  , onFinish :: Aff Unit
  }

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Newtype (Renderer msg sta) _

derive instance Newtype (Controller msg sta) _

