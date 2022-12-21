module Marionette.Renderers.Commander
  ( KeyboardUserInput(..)
  , NativeNodeKey
  , Output(..)
  , PureCompleter
  , CliSurface(..)
  , TextInput
  , defaultConfig
  , defaultTextInput
  , mkRenderer
  , mkRenderer_
  , noCliSurface
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Marionette.Types (Renderer(..))
import Node.ReadLine as RL

data CliSurface msg = CliSurface Output (KeyboardUserInput msg)

newtype Output = TextOutput String

data KeyboardUserInput msg
  = TextInput (String -> Maybe msg) TextInput
  | KeyInput String (NativeNodeKey -> Maybe msg)
  | NoInput

type PureCompleter = String -> { completions :: Array String, matched :: String }

type Config =
  { clearScreen :: Boolean
  , separator :: Maybe String
  , prompt :: String -> String
  , noPrompt :: String
  }

type TextInput =
  { prompt :: String
  , completions :: PureCompleter
  }

type NativeNodeKey =
  { sequence :: String
  , name :: String
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

type View msg sta = sta -> CliSurface msg

---

noCliSurface :: forall msg. CliSurface msg
noCliSurface = CliSurface (TextOutput "") NoInput

defaultTextInput :: TextInput
defaultTextInput = { prompt: "", completions: noCompletion }

noCompletion :: PureCompleter
noCompletion s = { completions: [], matched: s }

defaultConfig :: Config
defaultConfig =
  { clearScreen: true
  , separator: Nothing
  , noPrompt: ""
  , prompt: \text -> "> " <>
      if text == "" then "" else text
  }

mkRenderer_ :: forall msg sta. View msg sta -> Renderer msg sta
mkRenderer_ view = mkRenderer view defaultConfig

mkRenderer :: forall msg sta. View msg sta -> Config -> Renderer msg sta
mkRenderer view cfg = Renderer
  { onInit, onState, onFinish }
  where
  onInit = do
    when cfg.clearScreen do
      log eraseScreen
      log $ cursorMoveDown 200
    liftEffect emitKeypressEvents

  onState state onMsg = do
    let surface = view state
    renderCliSurface onMsg surface


  maybeSeperator = case cfg.separator of
    Just sep -> log sep
    Nothing -> pure unit

  renderCliSurface onMsg (CliSurface output input) = do
    when cfg.clearScreen (log eraseScreen)
    renderOutput output
    maybeSeperator
    renderInput onMsg input

  renderOutput (TextOutput txt) = log txt

  renderInput onMsg = case _ of
    NoInput -> log cfg.noPrompt

    TextInput mkMsg { prompt, completions } -> do
      log prompt
      answer <- getAnswer completions
      case mkMsg answer of
        Just msg -> onMsg msg
        Nothing -> pure unit

    KeyInput promptTxt mkMsg -> do
      log $ cfg.prompt promptTxt
      liftEffect $ getKey \key ->
        case mkMsg key of
          Just msg -> launchAff_ $ onMsg msg
          Nothing -> pure unit

  onFinish = log eraseScreen

---


eraseScreen :: Effect Unit
eraseScreen = log ("\x1b" <> "[2J")

cursorMoveDown :: Int ->  Effect Unit
cursorMoveDown n = log ("\x1b" <> "[" <> show n <> "B")

foreign import getKey :: (NativeNodeKey -> Effect Unit) -> Effect Unit

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
