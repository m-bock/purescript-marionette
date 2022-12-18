module Test.Examples.Snake.Data.CharGrid where

import Prelude

import Data.Array as Arr
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.String as Str
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Regex (replace) as Reg
import Data.String.Regex.Flags (noFlags) as Reg
import Data.String.Regex.Unsafe (unsafeRegex) as Reg
import Test.Examples.Snake.Data.Grid (ErrorFromArrays, Grid)
import Test.Examples.Snake.Data.Grid as Grid
import Test.Examples.Snake.Data.Vector (Vector(..))

type CharGrid = Grid Char

type Vec = Vector Int

toString :: CharGrid -> String
toString = Grid.toArrays >>> map fromCharArray >>> Str.joinWith "\n"

fromString :: String -> Either ErrorFromArrays CharGrid
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

writeText :: Vec -> String -> CharGrid -> CharGrid
writeText vec str cgrid = Grid.insertSubgridCropped vec cgrid' cgrid
  where
  cgrid' = str # toCharArray # Grid.fromArray

writeTextCenter :: String -> CharGrid -> CharGrid
writeTextCenter str cgrid =
  writeText vec str cgrid
  where
  vec = (div <$> Grid.size cgrid <*> Vec 2 2) - Vec (Str.length str / 2) 0