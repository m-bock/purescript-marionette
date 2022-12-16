module Marionette where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Marionette.Types (Renderer(..))
import Unsafe.Coerce (unsafeCoerce)

type Program msg sta =
  { initialState :: sta
  ,
    -- exitIf :: msg -> sta -> Boolean
    initialMsg :: Maybe msg
  --, onEvent :: ProgramEvent msg sta -> Effect Unit
  , renderer :: Renderer msg sta
  --, controller :: Control msg sta
  }

noRenderer :: forall msg sta. Renderer msg sta
noRenderer = Renderer
  { onInit: pure unit
  , onState: \_ _ -> pure unit
  , onFinish: pure unit
  }

defaultProgram :: Program Unit Unit
defaultProgram =
  { renderer: noRenderer
  , initialState: unit
  , initialMsg: Nothing
  }

-- data ProgramEvent msg sta = ProgramEvent Instant (EventType msg sta)

-- derive instance Generic (ProgramEvent msg sta) _

-- instance (Show msg, Show sta) => Show (ProgramEvent msg sta) where
--   show = genericShow

-- derive instance Generic (EventType msg sta) _

-- instance (Show msg, Show sta) => Show (EventType msg sta) where
--   show = genericShow

-- data EventType msg sta
--   = NewMsg ThreadSlotId msg
--   | EndMsg ThreadSlotId
--   | NewState ThreadSlotId sta

runProgram :: forall msg sta. Program msg sta -> Aff sta
runProgram cfg = do
  log "hello"
  pure cfg.initialState