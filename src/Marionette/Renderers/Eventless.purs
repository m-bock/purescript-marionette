-- | The 'eventless' renderer is a trivial renderer. In prints the stringified
-- | state to the console and does not handle any user input.

module Marionette.Renderers.Eventless
  ( Config
  , View
  , defaultConfig
  , mkRenderer
  , mkRenderer_
  ) where

import Prelude

import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Marionette.Types (Renderer(..))

--------------------------------------------------------------------------------
--- Public
--------------------------------------------------------------------------------

-- | Main configuration options for the `eventless` renderer
-- |
-- | - `clearScreen` defines weather the terminal screen should be cleared
-- |   before each render. If not, subsequent views are rendered in below each other in
-- |   the terminal.
type Config = { clearScreen :: Boolean }

-- | The actual high level view function that the user will provide
type View sta = sta -> String

-- | Some defaults for the [Config](#t:Config) type
defaultConfig :: Config
defaultConfig = { clearScreen: true }

-- | Creates an eventless renderer with configuration
mkRenderer :: forall sta msg. Config -> View sta -> Renderer msg sta
mkRenderer opts view = Renderer
  { onInit
  , onState
  , onFinish
  }
  where
  onInit _ = do
    when opts.clearScreen do
      eraseScreen
      cursorMoveDown 200

  onState sta _ = do
    when opts.clearScreen do
      eraseScreen
    log $ view sta

  onFinish = do
    when opts.clearScreen do
      eraseScreen

-- | Creates an eventless renderer
mkRenderer_ :: forall sta msg. View sta -> Renderer msg sta
mkRenderer_ = mkRenderer defaultConfig

--------------------------------------------------------------------------------
--- Utils
--------------------------------------------------------------------------------

eraseScreen :: forall m. MonadEffect m => m Unit
eraseScreen = log ("\x1b" <> "[2J")

cursorMoveDown :: forall m. MonadEffect m => Int -> m Unit
cursorMoveDown n = log ("\x1b" <> "[" <> show n <> "B")