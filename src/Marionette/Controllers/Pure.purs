-- | A simplistic controller that is completely pure and does not perform any side effects.

module Marionette.Controllers.Pure
  ( UpdateFn
  , mkController
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Marionette.Types (Controller(..), State(..))

--------------------------------------------------------------------------------
--- Public
--------------------------------------------------------------------------------

-- | Pure update Function
type UpdateFn msg sta = msg -> sta -> sta

-- | Creates a low level controller when given a pure update function
mkController :: forall msg sta. UpdateFn msg sta -> Controller msg sta
mkController update = Controller \_ (State state) msg -> do
  let modifyState_ = \f -> state \s -> Tuple unit (f s)
  modifyState_ $ update msg

