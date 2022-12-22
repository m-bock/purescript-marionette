module Test.Examples.WordTicker where

import Prelude

import Control.Monad.State (get, modify_)
import Data.Array as Arr
import Data.Maybe (Maybe(..))
import Data.String as Str
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Marionette as Mar
import Marionette.Controllers.Monadic (MarionetteT, sendMsg)
import Marionette.Controllers.Monadic as Mon
import Marionette.Renderers.Commander (CliSurface(..), Completions(..), KeyPrompt(..), KeyboardUserInput(..), Output(..), TextPrompt(..))
import Marionette.Renderers.Commander as Comm

data State
  = EnterWord
  | Greet { word :: String, time :: Int }

derive instance Eq State

data Msg
  = SetWord String
  | GoToEnterWord
  | Tick

initialState :: State
initialState = EnterWord

control :: forall m. MonadAff m => Msg -> MarionetteT Msg State m Unit
control = case _ of
  SetWord word -> do
    modify_ case _ of
      EnterWord -> Greet { word, time: 0 }
      st -> st
    sendMsg Tick

  Tick -> do
    modify_ case _ of
      Greet r -> Greet r { time = r.time + 1 }
      st -> st
    liftAff $ Aff.delay (Milliseconds 100.0)

    get >>= case _ of
      Greet _ -> sendMsg Tick
      _ -> pure unit

  GoToEnterWord -> do
    modify_ case _ of
      Greet _ -> EnterWord
      st -> st

view :: State -> CliSurface Msg
view = case _ of
  EnterWord -> CliSurface
    (TextOutput "")
    ( TextInput
        (TextPrompt "Enter a word" NoCompletions)
        (Just <<< SetWord)
    )

  Greet { word, time } -> CliSurface
    ( let
        sep = " ++ "
        width = 40
        line = (Str.joinWith sep $ Arr.replicate width word) <> sep
      in
        TextOutput $
          (Str.take width $ rotWord line time) <> "\n"

    )
    ( Comm.KeyInput
        (KeyPrompt "Press 'w' enter a new word!")
        case _ of
          { name: "w" } -> Just GoToEnterWord
          _ -> Nothing
    )

rotWord :: String -> Int -> String
rotWord str n = after <> before
  where
  { after, before } = Str.splitAt (n `mod` Str.length str) str

program :: Mar.Program Msg State
program =
  { initialState
  , renderer: Comm.mkRenderer_ view
  , controller: Mon.mkController control
  }

main :: Effect Unit
main = launchAff_ do
  _ <- Mar.runProgram program Mar.defaultConfig --{initialMsg = Just Tick}
  pure unit
