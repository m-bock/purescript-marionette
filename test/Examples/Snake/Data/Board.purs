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

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (foldr, traverse)
import Test.Examples.Snake.Data.CharGrid as CharGrid
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Grid (ErrorFromArrays, Grid, Vec)
import Test.Examples.Snake.Data.Grid as Grid
import Test.Examples.Snake.Data.Vector (Vector(..))
import Test.Examples.Snake.GridParser (GridParseError, GridParser)
import Test.Examples.Snake.GridParser as GP
import Unsafe.Coerce (unsafeCoerce)

data Tile
  = Tile_SnakeHead
  | Tile_SnakeBody
  | Tile_Goodie
  | Tile_Wall
  | Tile_Floor

data MazeTile
  = MazeTile_Floor
  | MazeTile_Wall

data Board = UnsafeBoard Board'

type Board' =
  { maze :: Maze
  , snake :: Snake
  , goodie :: Goodie
  }

unBoard :: Board -> Board'
unBoard (UnsafeBoard x) = x

data Snake = Snake Vec Vec (Array Vec)
data Goodie = Goodie Vec
newtype Maze = Maze (Grid MazeTile)

instance Show Board where
  show (UnsafeBoard i) = "board:" -- <> show i

derive instance Eq Board
derive instance Eq Goodie
derive instance Eq Maze
derive instance Eq MazeTile
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
  lmap ErrGridPArse $ GP.runParser tileBoard parseBoard

parseBoard :: GridParser Tile Board
parseBoard = ado
  maze <- parseMaze
  snake <- parseSnake
  goodie <- parseGoodie
  parseRest
  in UnsafeBoard { maze, snake, goodie }

parseMaze :: GridParser Tile Maze
parseMaze = Maze <$> GP.scanGrid MazeTile_Floor \_ -> case _ of
  Tile_Wall -> Just MazeTile_Wall
  _ -> Nothing

parseSnake :: GridParser Tile Snake
parseSnake = ado
  --GP.setDirection Dir.Right

  GP.moveTo (\_ -> (_ == Tile_SnakeHead))
  h <- GP.position <* GP.any
  n <- GP.position <* GP.satisfies (\_ -> (_ == Tile_SnakeBody))
  b1 <- GP.position <* GP.satisfies (\_ -> (_ == Tile_SnakeBody))
  b2 <- GP.position <* GP.satisfies (\_ -> (_ == Tile_SnakeBody))
  b3 <- GP.position <* GP.satisfies (\_ -> (_ == Tile_SnakeBody))
  in Snake h n [ b1, b2, b3 ]

parseRest :: GridParser Tile Unit
parseRest = void $ GP.scanGrid unit \_ -> case _ of
  Tile_Wall -> Just unit
  _ -> Nothing

parseGoodie :: GridParser Tile Goodie
parseGoodie = ado
  GP.moveTo (\_ -> (_ == Tile_Goodie))
  g <- GP.position <* GP.any
  in Goodie g

parseChar :: Char -> Either ParseError Tile
parseChar = case _ of
  '#' -> Right Tile_Wall
  'O' -> Right Tile_SnakeBody
  '+' -> Right Tile_SnakeHead
  'x' -> Right Tile_Goodie
  ' ' -> Right Tile_Floor
  c -> Left $ ErrInvalidChar c

--- Printer

type GridPrinter b a = a -> Grid b -> Grid b

type GridPrinter_ b a = a -> Grid b

printBoard :: GridPrinter_ Tile Board
printBoard = unBoard >>> \{ maze, goodie, snake } ->
  printMaze maze
    # printGoodie goodie
    # printSnake snake

printMaze :: GridPrinter_ Tile Maze
printMaze (Maze grid) = grid <#> case _ of
  MazeTile_Wall -> Tile_Wall
  MazeTile_Floor -> Tile_Floor

printSnake :: GridPrinter Tile Snake
printSnake (Snake head neck tail) grid = grid
  # Grid.insert' head Tile_SnakeHead
  # Grid.insert' neck Tile_SnakeBody
  # \g -> foldr (\pos -> Grid.insert' pos Tile_SnakeBody) g tail

printGoodie :: GridPrinter Tile Goodie
printGoodie (Goodie pos) grid = grid
  # Grid.insert pos Tile_Goodie
  # fromMaybe grid

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
setDirection _ = unsafeCoerce 1

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

x
  :: forall a188 b189
   . { direction :: b189 -> Either a188 b189
     , grid ::
         { entries :: Array (Array (Maybe Tile))
         , size :: Vector Int
         }
     , position :: Vector Int
     }
x =
  { direction: Right
  , grid:
      { entries:
          [ [ (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), Nothing, Nothing, Nothing, Nothing, (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall) ]
          , [ (Just Tile_Wall), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, (Just Tile_Wall) ]
          , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, (Just Tile_Wall) ]
          , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, (Just Tile_Wall) ]
          , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, (Just Tile_Wall) ]
          , [ (Just Tile_Wall), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, (Just Tile_Wall) ]
          , [ (Just Tile_Wall), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
          , [ (Just Tile_Wall), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
          , [ (Just Tile_Wall), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, (Just Tile_Wall) ]
          , [ (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall), (Just Tile_Wall) ]
          ]
      , size: (Vec 20 10)
      }
  , position: (Vec 0 0)
  }