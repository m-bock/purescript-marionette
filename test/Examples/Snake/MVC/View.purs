module Test.Examples.Snake.MVC.View where

import Prelude

import Data.Maybe (Maybe(..))
import Marionette.Renderers.Commander (KeyboardUserInput(..), Output(..))
import Marionette.Renderers.Commander as Com
import Test.Examples.Snake.Board (Tile(..))
import Test.Examples.Snake.Board as Board
import Test.Examples.Snake.Data.CharGrid as CharGrid
import Test.Examples.Snake.Data.Direction as Dir
import Test.Examples.Snake.Data.Grid as Grid
import Test.Examples.Snake.Data.Vector (Vector(..))
import Test.Examples.Snake.MVC.Model (Game(..), Msg(..), State(..))

view :: State -> Com.Surface Msg
view = case _ of
  State_Init -> Com.Surface
    (TextOutput "")
    ( KeyInput
        ( case _ of
            { name: "s" } -> Just Msg_Start
            _ -> Nothing
        )
        { prompt: "Press 's' to start!" }
    )

  State_Playing (Game { board }) -> Com.Surface
    ( TextOutput $
        board
          # Board.toGrid
          <#>
            case _ of
              Tile_Wall -> '#'
              Tile_Floor -> ' '
              Tile_Goodie -> 'x'
              Tile_SnakeHead -> '+'
              Tile_SnakeBody -> 'O'
          # CharGrid.toString
    )
    ( KeyInput
        ( case _ of
            { name: "up" } -> Just $ Msg_Navigate Dir.Up
            { name: "right" } -> Just $ Msg_Navigate Dir.Right
            { name: "down" } -> Just $ Msg_Navigate Dir.Down
            { name: "left" } -> Just $ Msg_Navigate Dir.Left

            _ -> Nothing
        )
        { prompt: "Up/Right/Down/Left" }
    )

  State_Error msg -> Com.Surface
    (TextOutput $ "Error: " <> show msg)
    NoInput

  State_Paused _ -> Com.Surface
    (TextOutput "Paused")
    NoInput

  State_Lost _ -> Com.Surface
    (TextOutput "Lost")
    NoInput

  State_Won _ -> Com.Surface
    ( TextOutput $
        Grid.fill (const '*') (Vec 20 10)
          # CharGrid.writeTextCenter "Hallo"
          # CharGrid.toString
    )
    NoInput
