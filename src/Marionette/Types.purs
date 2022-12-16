module Marionette.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)

newtype State s m = State (forall a. (s -> (Tuple a s)) -> m a)

type SendMsg msg m = (msg -> m Unit)

newtype Controller msg sta = Controller (SendMsg msg Aff -> State sta Aff -> msg -> Aff Unit)

newtype Renderer msg sta = Renderer
  { onInit :: Effect Unit
  , onState :: sta -> (msg -> Aff Unit) -> Aff Unit
  , onFinish :: Aff Unit
  }

derive instance Newtype (Renderer msg sta) _

derive instance Newtype (Controller msg sta) _

