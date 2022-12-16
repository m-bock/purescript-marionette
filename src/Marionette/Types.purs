module Marionette.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)

type State s m = forall a. (s -> (Tuple a s)) -> m a

type SendMsg msg m = (msg -> m Unit)

newtype Controller msg sta = Controller (SendMsg msg Aff -> State sta Aff -> msg -> Aff (Maybe msg))

newtype Renderer msg sta = Renderer
  { onInit :: Effect Unit
  , onState :: sta -> (msg -> Aff Unit) -> Aff Unit
  , onFinish :: Aff Unit
  }
