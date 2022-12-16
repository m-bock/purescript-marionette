module Marionette.Renderers.Commander
  ( KeyInput
  , KeyboardUserInput(..)
  , NativeNodeKey
  , PureCompleter
  , Surface(..)
  , TextInput
  , defaultConfig
  , defaultKeyInput
  , defaultTextInput
  , mkRenderer
  , mkRenderer_
  , noSurface
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Marionette.Types (Renderer(..))
import Node.ReadLine as RL

---

data Surface msg = Surface String (KeyboardUserInput msg)

noSurface :: forall msg. Surface msg
noSurface = Surface "" NoInput

type TextInput =
  { prompt :: String
  , completions :: PureCompleter
  }

defaultTextInput :: TextInput
defaultTextInput = { prompt: "", completions: noCompletion }

noCompletion :: PureCompleter
noCompletion s = { completions: [], matched: s }

type KeyInput = { prompt :: String }

defaultKeyInput :: KeyInput
defaultKeyInput = { prompt: "" }

type PureCompleter = String -> { completions :: Array String, matched :: String }

data KeyboardUserInput msg
  = TextInput (String -> msg) TextInput
  | KeyInput (NativeNodeKey -> msg) KeyInput
  | NoInput

type Config =
  { clearScreen :: Boolean
  , separator :: Maybe String
  , prompt :: String -> String
  , noPrompt :: String
  }

defaultConfig :: Config
defaultConfig =
  { clearScreen: true
  , separator: Nothing
  , noPrompt: "#"
  , prompt: \text -> "> " <>
      if text == "" then "" else text <> " :"
  }

type NativeNodeKey =
  { sequence :: String
  , name :: String
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

type View msg sta = sta -> Surface msg

mkRenderer_ :: forall msg sta. View msg sta -> Renderer msg sta
mkRenderer_ = mkRenderer defaultConfig

mkRenderer :: forall msg sta. Config -> View msg sta -> Renderer msg sta
mkRenderer cfg view = Renderer
  { onInit, onState, onFinish: log eraseScreen }
  where
  onInit = liftEffect emitKeypressEvents

  onState state onMsg = do
    let surface = view state
    renderSurface onMsg surface

  maybeClear = when cfg.clearScreen (log eraseScreen)

  maybeSeperator = case cfg.separator of
    Just sep -> log sep
    Nothing -> pure unit

  renderSurface onMsg (Surface output input) = do
    maybeClear
    renderOutput output
    maybeSeperator
    renderInput onMsg input

  renderOutput = log

  renderInput onMsg = case _ of
    NoInput -> log cfg.noPrompt
    TextInput mkMsg { prompt, completions } -> do
      log prompt
      answer <- getAnswer completions
      let msg = mkMsg answer
      onMsg msg
    KeyInput mkMsg { prompt } -> do
      log prompt
      key <- getKey
      let msg = mkMsg key
      onMsg msg

---

eraseScreen :: String
eraseScreen = "\x1b" <> "[2J"

foreign import getKeyImpl :: Effect (Promise NativeNodeKey)

foreign import emitKeypressEvents :: Effect Unit

getAnswer :: PureCompleter -> Aff String
getAnswer completions = do
  interface <- liftEffect $ RL.createConsoleInterface (pure <<< completions)
  answer <- promptAff interface
  liftEffect $ RL.close interface
  pure answer

promptAff
  :: RL.Interface -> Aff String
promptAff interface = makeAff \handler -> do
  RL.setLineHandler (handler <<< Right) interface
  RL.prompt interface
  pure nonCanceler

getKey :: Aff NativeNodeKey
getKey = toAffE getKeyImpl
