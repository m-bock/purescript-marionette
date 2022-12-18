module Test.Examples.Snake.MVC.View where

import Prelude

import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Debug (spy, spyWith)
import Marionette.Renderers.Commander (KeyboardUserInput(..), defaultKeyInput)
import Marionette.Renderers.Commander as Com
import Test.Examples.Snake.Data.Board (Tile(..))
import Test.Examples.Snake.Data.Board as Board
import Test.Examples.Snake.Data.CharGrid (CharGrid)
import Test.Examples.Snake.Data.CharGrid as CharGrid
import Test.Examples.Snake.Data.Grid as Grid
import Test.Examples.Snake.Data.Vector (Vector(..))
import Test.Examples.Snake.MVC.Model (Game(..), Msg(..), State(..))

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

  Sta_Playing (Game { board }) -> Com.Surface
    ( board
        # Board.toGrid
        <#>
          case _ of
            Tile_Wall -> '#'
            Tile_Floor -> ' '
            _ -> '?'
        # CharGrid.toString
    )
    NoInput

  Sta_Error msg -> Com.Surface
    ("Error: " <> show msg)
    NoInput

  Sta_Paused _ -> Com.Surface
    "Paused"
    NoInput

  Sta_Lost _ -> Com.Surface
    "Lost"
    NoInput

  Sta_Won _ -> Com.Surface
    ( Grid.fill (const '*') (Vec 20 10)
        # CharGrid.writeTextCenter "Hallo"
        # CharGrid.toString
    )
    NoInput
