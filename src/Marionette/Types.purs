module Marionette.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)

type State s m = forall a. (s -> (Tuple a s)) -> m a

type SendMsg msg m = (msg -> m Unit)

type Control msg sta = SendMsg msg Aff  -> State sta Aff -> msg -> Aff (Maybe msg)