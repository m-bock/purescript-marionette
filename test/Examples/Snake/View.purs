module Test.Examples.Snake.View where

import Prelude

import Marionette.Renderers.Commander (KeyboardUserInput(..), defaultKeyInput)
import Marionette.Renderers.Commander as Com
import Test.Examples.Snake.Model (Msg(..), State(..))

view :: State -> Com.Surface Msg
view = case _ of
  Sta_Init -> Com.Surface
    ""
    ( KeyInput
        ( case _ of
            { name: "s" } -> Msg_Start
            _ -> Msg_NoOp
        )
        { prompt: "Press 's' to start!" }
    )
  _ -> Com.Surface "..." NoInput