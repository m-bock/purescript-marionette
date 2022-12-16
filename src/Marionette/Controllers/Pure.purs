module Marionett.ControlHandlers.Pure
  ( UpdateFn
  , mkController
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Marionette.Types (Controller(..))

type UpdateFn msg sta = msg -> sta -> sta

---

mkController :: forall msg sta. UpdateFn msg sta -> Controller msg sta
mkController update = Controller \_ state msg -> do
  let modifyState_ = \f -> state \s -> Tuple unit (f s)
  modifyState_ $ update msg
  pure Nothing

