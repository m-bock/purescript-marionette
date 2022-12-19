module Marionette.Renderers.Eventless
  ( Config
  , defaultConfig
  , mkRenderer
  , mkRenderer_
  ) where

import Prelude

import Effect.Class.Console (log)
import Marionette.Types (Renderer(..))

type Config = { clearScreen :: Boolean }
type View sta = sta -> String

defaultConfig :: Config
defaultConfig = { clearScreen: true }

mkRenderer :: forall sta msg. Config -> View sta -> Renderer msg sta
mkRenderer opts view = Renderer
  { onInit: pure unit
  , onState: \sta _ -> maybeClear <* (log $ view sta)
  , onFinish: maybeClear
  }
  where
  maybeClear = when opts.clearScreen (log eraseScreen)

mkRenderer_ :: forall sta msg. View sta -> Renderer msg sta
mkRenderer_ = mkRenderer defaultConfig

---

eraseScreen :: String
eraseScreen = "\x1b" <> "[2J"