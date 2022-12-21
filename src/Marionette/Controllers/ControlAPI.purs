-- | A Control handler that runs in `Aff` and provides a beginner friendly API
-- | to handle state updates with effects.

module Marionette.Controllers.ControlAPI
  ( Control
  , ControlAPI
  , mkController
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Marionette.Types (Controller(..), State(..))

--------------------------------------------------------------------------------
--- Public
--------------------------------------------------------------------------------

-- | The high level control function. Takes an interface to a control API
-- | and a `msg` and returns an effectful computation inside `Aff`. 
type Control msg sta = ControlAPI msg sta -> msg -> Aff Unit

-- | Provides read/write access to the current state and a way to raise new
-- | messages at any time in the control handler
-- |
-- | - `sendMsg` Raise a new message.
-- | - `getState` Set the current state.
-- | - `putState` Set the state.
-- | - `modifyState` Modify the state by applying a function to the current state. The returned value is the new state value.
-- | - `modifyState_` Modify the state by applying a function to the current state.

type ControlAPI msg sta =
  { sendMsg :: msg -> Aff Unit
  , getState :: Aff sta
  , putState :: sta -> Aff Unit
  , modifyState :: (sta -> sta) -> Aff sta
  , modifyState_ :: (sta -> sta) -> Aff Unit
  }

-- | Creates a low level controller when given a high level control function
mkController :: forall msg sta. Control msg sta -> Controller msg sta
mkController control = Controller \sendMsg_ (State state) msg ->
  let
    api =
      { sendMsg: sendMsg_
      , getState: state \s -> Tuple s s
      , putState: \s -> state \_ -> Tuple unit s
      , modifyState: \f -> state \s -> let s' = f s in Tuple s' s'
      , modifyState_: \f -> state \s -> Tuple unit (f s)
      }
  in
    control api msg
