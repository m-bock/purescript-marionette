module Marionett.ControlHandlers
  ( RecordAPI
  , recordApiControlHandler
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Marionette.Types (ControlHandler)

type RecordApiControlHandler msg sta = RecordAPI msg sta -> msg -> Aff (Maybe msg)

---

type RecordAPI msg sta =
  { sendMsg :: msg -> Aff Unit
  , getState :: Aff sta
  , putState :: sta -> Aff Unit
  , modifyState :: (sta -> sta) -> Aff sta
  , modifyState_ :: (sta -> sta) -> Aff Unit
  }

recordApiControlHandler :: forall msg sta. RecordApiControlHandler msg sta -> ControlHandler msg sta
recordApiControlHandler control sendMsg_ state msg =
  control api msg
  where
  api =
    { sendMsg: sendMsg_
    , getState: state \s -> Tuple s s
    , putState: \s -> state \_ -> Tuple unit s
    , modifyState: \f -> state \s -> let s' = f s in Tuple s' s'
    , modifyState_: \f -> state \s -> Tuple unit (f s)
    }