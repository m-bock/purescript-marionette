module Test.Examples.Snake.Data.Board
  ( Board
  , Goodie(..)
  , ParseError(..)
  , RandInt(..)
  , Tile(..)
  , next
  , parse
  , print
  , setDirection
  , toGrid
  ) where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, enumFromTo)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Test.Examples.Snake.Data.CharGrid as CharGrid
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Grid (ErrorFromArrays, Grid)
import Test.Examples.Snake.Data.Grid as Grid
import Test.Examples.Snake.Data.Vector (Vector(..))
import Test.Examples.Snake.GridParser (GridParseError, GridParser)
import Test.Examples.Snake.GridParser as GridParser
import Unsafe.Coerce (unsafeCoerce)

data Tile
  = Tile_SnakeHead
  | Tile_SnakeBody
  | Tile_Goodie
  | Tile_Wall
  | Tile_Floor

data MazeTile = MazeTile_Floor | MazeTile_Wall

data Board = UnsafeBoard Board'

type Board' =
  { maze :: Maze
  , snake :: Snake
  , goodie :: Goodie
  }

unBoard :: Board -> Board'
unBoard (UnsafeBoard x) = x

data Snake = Snake
data Goodie = Goodie
data Maze = Maze

instance Show Board where
  show (UnsafeBoard i) = "board:" -- <> show i

derive instance Eq Board
derive instance Eq Goodie
derive instance Eq Maze
derive instance Eq Snake

--- Parser

data ParseError
  = ErrFromArrays ErrorFromArrays
  | ErrInvalidChar Char
  | ErrGridPArse GridParseError

derive instance Eq ParseError

derive instance Generic ParseError _

instance Show ParseError where
  show = genericShow


parse :: String -> Either ParseError Board
parse str = do
  charBoard <- lmap ErrFromArrays $ CharGrid.fromString str
  tileBoard <- traverse parseChar charBoard
  lmap ErrGridPArse $ GridParser.runParser tileBoard parseBoard

parseBoard :: GridParser Tile Board
parseBoard = ado
  maze <- parseMaze
  snake <- parseSnake
  goodie <- parseGoodie
  parseRest
  in UnsafeBoard { maze, snake, goodie }

parseMaze :: GridParser Tile Maze
parseMaze = pure Maze

parseSnake :: GridParser Tile Snake
parseSnake = pure Snake

parseRest :: GridParser Tile Unit
parseRest = pure unit

parseGoodie :: GridParser Tile Goodie
parseGoodie = pure Goodie

parseChar :: Char -> Either ParseError Tile
parseChar = case _ of
  '#' -> Right Tile_Wall
  'O' -> Right Tile_SnakeBody
  '+' -> Right Tile_SnakeHead
  ' ' -> Right Tile_Floor
  c -> Left $ ErrInvalidChar c

-- findSnake :: Grid Tile -> Maybe Snake
-- findSnake grid = grid
--   # Grid.toUnfoldable
--   # Arr.filter (\(Tuple k v) -> v == Tile_SnakeHead )

--- Printer

type GridPrinter b a = a -> Grid b -> Grid b

type GridPrinter_ b a = a -> Grid b

printBoard :: GridPrinter_ Tile Board
printBoard _ = Grid.empty

printMaze :: GridPrinter_ Tile Maze
printMaze _ = Grid.empty

printSnake :: GridPrinter Tile Snake
printSnake _ = identity

printGoodie :: GridPrinter Goodie Snake
printGoodie _ = identity

---

print :: Board -> String
print board = ""

toGrid :: Board -> Grid Tile
toGrid = printBoard

data RandInt = RandInt Int

next :: RandInt -> Board -> Maybe Board
next _ (UnsafeBoard n) = Just $ UnsafeBoard (n)

-- getFreeSpots :: Board -> Array (Vector Int)
-- getFreeSpots = unsafeCoerce 1

setDirection :: Direction -> Board -> Maybe Board
setDirection = unsafeCoerce 1

---

derive instance Generic Tile _

derive instance Eq Tile

instance Show Tile where
  show = genericShow

---

-- data ABC = A | B | C

-- derive instance Ord ABC
-- derive instance Eq ABC

-- f :: ABC -> String
-- f = case _ of
--   A -> "A"
--   B -> "B"
--   C -> "C"

-- mk :: forall a b. BoundedEnum a => Ord b => (a -> b) -> (b -> Maybe a)
-- mk f =
--   let
--     l = (enumFromTo bottom top :: Array _)
--       <#> (\x -> Tuple (f x) x)
--       # Map.fromFoldable
--   in
--     \k -> Map.lookup k l

-- x = [ A, B, C ] <#> (\x -> Tuple (f x) x) # Map.fromFoldable

-- gg :: String -> Maybe ABC
-- gg = mk f

-- g :: String -> Maybe ABC
-- g k = Map.lookup k x
