module Test.Examples.Snake.View where

import Prelude

import Data.Maybe (Maybe(..))
import Marionette.Renderers.Commander (KeyboardUserInput(..), defaultKeyInput)
import Marionette.Renderers.Commander as Com
import Test.Examples.Snake.Model (Msg(..), State(..))

view :: State -> Com.Surface Msg
view = case _ of
  Sta_Init -> Com.Surface
    ""
    ( KeyInput
        ( case _ of
            { name: "s" } -> Just Msg_Start
            _ -> Nothing
        )
        { prompt: "Press 's' to start!" }
    )

  Sta_Playing game -> Com.Surface
    "playing"
    NoInput

  Sta_Error msg -> Com.Surface
    ("Error: " <> msg)
    NoInput

  _ -> Com.Surface "..." NoInput