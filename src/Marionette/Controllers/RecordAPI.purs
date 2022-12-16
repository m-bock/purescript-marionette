module Marionett.ControlHandlers
  ( RecordAPI
  , mkController
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Marionette.Types (Controller(..), State(..))

type Control msg sta = RecordAPI msg sta -> msg -> Aff Unit

type RecordAPI msg sta =
  { sendMsg :: msg -> Aff Unit
  , getState :: Aff sta
  , putState :: sta -> Aff Unit
  , modifyState :: (sta -> sta) -> Aff sta
  , modifyState_ :: (sta -> sta) -> Aff Unit
  }

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
