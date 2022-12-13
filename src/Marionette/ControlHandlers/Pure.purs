module Marionett.ControlHandlers.Pure
  ( PureControlHandler
  , pureControlHandler
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Marionette.Types (ControlHandler)

type PureControlHandler msg sta = msg -> sta -> sta

---

pureControlHandler :: forall msg sta. PureControlHandler msg sta -> ControlHandler msg sta
pureControlHandler update _ state msg = do
  modifyState_ $ update msg
  pure Nothing
  where
  modifyState_ = \f -> state \s -> Tuple unit (f s)

