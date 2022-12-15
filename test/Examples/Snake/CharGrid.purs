module Test.Examples.Snake.CharGrid where

import Prelude

import Data.Maybe (Maybe)
import Data.String as Str
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Regex (replace) as Reg
import Data.String.Regex.Flags (noFlags) as Reg
import Data.String.Regex.Unsafe (unsafeRegex) as Reg
import Test.Examples.Snake.Grid (Grid)
import Test.Examples.Snake.Grid as Grid

type CharGrid = Grid Char

toString :: CharGrid -> String
toString = Grid.toArrays >>> map fromCharArray >>> Str.joinWith "\n"

fromString :: String -> Maybe CharGrid
fromString = trimNewlines
  >>> Str.split (Str.Pattern "\n")
  >>> map toCharArray
  >>> Grid.fromArrays

trimNewlines :: String -> String
trimNewlines =
  Reg.replace leadingNewlines "" >>> Reg.replace trailingNewlines ""
  where
  leadingNewlines = Reg.unsafeRegex "^\\n*" Reg.noFlags
  trailingNewlines = Reg.unsafeRegex "\\n*$" Reg.noFlags


