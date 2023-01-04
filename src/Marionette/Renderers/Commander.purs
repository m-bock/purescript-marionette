-- | `commander` is a `marionette` renderer that aims to unite the best of both
-- | worlds: declarative user interfaces and terminal applications.
-- | 
-- | Visually and logically a `commander` program is split up horizontally into two sections:
-- |  - **output section**
-- |
-- |    arbitrary size, just text output rendered to the screen
-- |  - **input section**
-- |
-- |    It typically consists of two lines:
-- |      - **prompt line** that prompts the user for input (e.g. "Press the
-- |        's' key to start" or "Enter your name")
-- |      - **command line** that holds the cursor an receives user input
-- |      
-- | One characteristic is that the cursor will always stay in the lowermost
-- | line. You'll not find the cursor in some input fields scattered around the
-- | screen.

module Marionette.Renderers.Commander
  ( CliSurface(..)
  , Completions(..)
  , Config
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

derive newtype instance Semigroup Output
derive newtype instance Monoid Output

-- | In a `commander` program the way user input is received is constrained to
-- | the following options
data KeyboardUserInput msg
  -- | Arbitrary length text input (e.g. "hello")
  = TextInput TextPrompt (String -> Maybe msg)
  -- | Input from a single keystroke (e.g. `left`, `return` or `ctrl + a`)
  | KeyInput KeyPrompt (NativeNodeKey -> Maybe msg)
  -- | Used for sceens without user input
  | NoInput

-- | The prompt text used when the user should enter a key
newtype KeyPrompt = KeyPrompt String

-- | The prompt text used when the user should enter some text
data TextPrompt = TextPrompt String Completions

-- | Text input can optionally define a completer function
data Completions
  = Completions (String -> { completions :: Array String, matched :: String })
  | NoCompletions

-- | Main configuration options for the `commander` renderer
-- | - `clearScreen` defines weather the terminal screen should be cleared
-- |   before each render. If not, subsequent views are rendered below each other in
-- |   the terminal.
-- | - `separator` optionally render a separator between the output and the
-- |   input section. E.g. `------------`
-- | - `prompt` sets the rendering of the prompt line. E.g. `\text -> "> " <>
-- |   text`
-- | - `noPrompt` defines what should be rendered if there is no user input prompt.
type Config =
  { clearScreen :: Boolean
  , separator :: Maybe String
  , prompt :: String -> String
  , noPrompt :: String
  }

-- | When a key event is received the following native nodejs key event can be
-- | handled.
-- | In the future this may change in favor of a parsed representation.
-- | Unfortunately there seems to be no official listing of possible key names,
-- |
-- | So here are some common ones:
-- | `up`/`down`/`right`/`left`/`a`/`b`/`c`/`1`/`2`/`3`/`space`/`f1`/`f2`/`f3`/`backspace`/`return`
-- |
-- | You can use `node -e 'readline.emitKeypressEvents(process.stdin);
-- | process.stdin.setRawMode(true).on("keypress", console.log)'` to investigate more.
type NativeNodeKey =
  { sequence :: String
  , name :: String
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  }

-- | The actual high level view function that the user will provide
type View msg sta = sta -> CliSurface msg

-- | Shorthand for an empty and eventless surface
noCliSurface :: forall msg. CliSurface msg
noCliSurface = CliSurface (TextOutput "") NoInput

-- | Some defaults for the [Config](#t:Config) type
defaultConfig :: Config
defaultConfig =
  { clearScreen: true
  , separator: Nothing
  , noPrompt: ""
  , prompt: \text -> "> " <> text
  }

-- | Creates an `commander` renderer from a view function
mkRenderer_ :: forall msg sta. View msg sta -> Renderer msg sta
mkRenderer_ view = mkRenderer view defaultConfig

-- | Creates an `commander` renderer from a view function with configuration
mkRenderer :: forall msg sta. View msg sta -> Config -> Renderer msg sta
mkRenderer view cfg = Renderer
  { onInit, onState, onFinish }
  where
  onInit _ = do
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