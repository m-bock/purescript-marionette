module Test.Examples.Snake.View where

import Prelude

import Test.Examples.Snake.Model (State)

view :: State -> String
view = show