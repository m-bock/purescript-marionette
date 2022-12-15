module Marionette where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)


type ProgramConfig msg sta =
  { 
    -- exitIf :: msg -> sta -> Boolean
  --, initialMsg :: Maybe msg
  --, onEvent :: ProgramEvent msg sta -> Effect Unit
  --, render :: Render msg sta
  --, control :: Control msg sta
  }


defaultProgramConfig :: forall msg sta. ProgramConfig msg sta
defaultProgramConfig = {}

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



runProgram :: forall msg sta. sta -> ProgramConfig msg sta -> Aff sta
runProgram sta _ = do
  log "hello"
  pure sta