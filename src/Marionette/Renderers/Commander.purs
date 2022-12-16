module Marionette.Renderers.Commander
  ( KeyInput
  , KeyboardUserInput
  , PureCompleter
  , Surface
  , TextInput
  , mkRenderer
  , defaultConfig
  , defaultKeyInput
  , defaultTextInput
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

type Surface key msg =
  { output :: String
  , input :: KeyboardUserInput key msg
  }

noSurface :: forall msg. Surface String msg
noSurface =
  { output: ""
  , input: NoInput
  }

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

data KeyboardUserInput key msg
  = TextInput (String -> msg) TextInput
  | KeyInput (key -> msg) KeyInput
  | NoInput

type Config key =
  { keyParser :: NativeNodeKey -> key
  , clearScreen :: Boolean
  , separator :: Maybe String
  , prompt :: String -> String
  , noPrompt :: String
  }

defaultConfig :: Config NativeNodeKey
defaultConfig =
  { keyParser: identity
  , clearScreen: true
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

type View key msg sta = sta -> Surface key msg

mkRenderer :: forall key msg sta. Config key -> View key msg sta -> Renderer msg sta
mkRenderer cfg view = Renderer
  { onInit, onState, onFinish: pure unit }
  where
  onInit = emitKeypressEvents

  onState state onMsg = do
    let surface = view state
    renderSurface onMsg surface

  maybeClear = when cfg.clearScreen (log eraseScreen)

  maybeSeperator = case cfg.separator of
    Just sep -> log sep
    Nothing -> pure unit

  renderSurface onMsg { output, input } = do
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
      let msg = mkMsg $ cfg.keyParser key
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
