module Marionette.Renderers.Commander
  ( CliSurface(..)
  , Completions(..)
  , KeyPrompt(..)
  , KeyboardUserInput(..)
  , NativeNodeKey
  , Output(..)
  , TextPrompt(..)
  , View
  , defaultConfig
  , mkRenderer
  , mkRenderer_
  , noCliSurface
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Marionette.Types (Renderer(..))
import Node.ReadLine as RL

--------------------------------------------------------------------------------
--- Public
--------------------------------------------------------------------------------

-- | The rendered surface in the terminal. Equivalent to `Html msg` types in Web frameworks.
-- | 
-- | It's responsible for both
-- | - render output to the console
-- | - handle user input
data CliSurface msg = CliSurface Output (KeyboardUserInput msg)

-- | Text output printed to the console
newtype Output = TextOutput String

data KeyboardUserInput msg
  = TextInput TextPrompt (String -> Maybe msg)
  | KeyInput KeyPrompt (NativeNodeKey -> Maybe msg)
  | NoInput

-- | The prompt text used when the user should enter a key
newtype KeyPrompt = KeyPrompt String

-- | The prompt text used when the user should enter some text
data TextPrompt = TextPrompt String Completions

data Completions
  = Completions (String -> { completions :: Array String, matched :: String })
  | NoCompletions

type Config =
  { clearScreen :: Boolean
  , separator :: Maybe String
  , prompt :: String -> String
  , noPrompt :: String
  }

type NativeNodeKey =
  { sequence :: String
  , name :: String
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

type View msg sta = sta -> CliSurface msg

noCliSurface :: forall msg. CliSurface msg
noCliSurface = CliSurface (TextOutput "") NoInput

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
      eraseScreen
      cursorMoveDown 200
    liftEffect emitKeypressEvents

  onState state onMsg = do
    let surface = view state
    renderCliSurface onMsg surface

  maybeSeperator = case cfg.separator of
    Just sep -> log sep
    Nothing -> pure unit

  renderCliSurface onMsg (CliSurface output input) = do
    when cfg.clearScreen eraseScreen
    renderOutput output
    maybeSeperator
    renderInput onMsg input

  renderOutput (TextOutput txt) = log txt

  renderInput onMsg = case _ of
    NoInput -> log cfg.noPrompt

    TextInput (TextPrompt promptTxt completions) mkMsg -> do
      log promptTxt
      answer <- getAnswer completions
      case mkMsg answer of
        Just msg -> onMsg msg
        Nothing -> pure unit

    KeyInput (KeyPrompt promptTxt) mkMsg -> do
      log $ cfg.prompt promptTxt
      liftEffect $ getKey \key ->
        case mkMsg key of
          Just msg -> launchAff_ $ onMsg msg
          Nothing -> pure unit

  onFinish = eraseScreen

---

eraseScreen :: forall m. MonadEffect m => m Unit
eraseScreen = log ("\x1b" <> "[2J")

cursorMoveDown :: forall m. MonadEffect m => Int -> m Unit
cursorMoveDown n = log ("\x1b" <> "[" <> show n <> "B")

getAnswer :: Completions -> Aff String
getAnswer pureCompletions = do
  interface <- liftEffect $ RL.createConsoleInterface completer
  answer <- promptAff interface
  liftEffect $ RL.close interface
  pure answer

  where
  completer = case pureCompletions of
    NoCompletions -> RL.noCompletion
    Completions f -> pure <<< f

promptAff
  :: RL.Interface -> Aff String
promptAff interface = makeAff \handler -> do
  RL.setLineHandler (handler <<< Right) interface
  RL.prompt interface
  pure nonCanceler

--------------------------------------------------------------------------------
--- FFI
--------------------------------------------------------------------------------

foreign import getKey :: (NativeNodeKey -> Effect Unit) -> Effect Unit

foreign import emitKeypressEvents :: Effect Unit