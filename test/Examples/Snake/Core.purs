module Test.Examples.Snake.Core
  ( Board(..)
  , Goodie(..)
  , LevelSpec(..)
  , Maze(..)
  , MazeItem(..)
  , Snake(..)
  , Tile(..)
  , Vec
  , applySnake
  , boardToMaze
  , findFreeSpots
  , findSnake
  , findSnakeDirection
  , mazeToBoard
  , mkBoard
  , parseLevelSpec
  , printBoard
  )
  where

import Prelude

import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), note)
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, un)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Test.Examples.Snake.Data.CharGrid as CharGrid
import Test.Examples.Snake.Data.Direction (Direction)
import Test.Examples.Snake.Data.Direction as Dir
import Test.Examples.Snake.Data.Grid (Grid)
import Test.Examples.Snake.Data.Grid as Grid
import Test.Examples.Snake.Data.Vector (Vector)
import Unsafe.Coerce (unsafeCoerce)

--- Vec 

type Vec = Vector Int

--- Tile

data Tile
  = Tile_SnakeHead
  | Tile_SnakeBody
  | Tile_Goodie
  | Tile_Wall
  | Tile_Floor

derive instance Generic Tile _

derive instance Eq Tile

instance Show Tile where
  show = genericShow

--

newtype Board = Board (Grid Tile)

derive instance Newtype Board _

data Snake = Snake Vec (Array Vec)

derive instance Generic Snake _

instance Show Snake where
  show = genericShow

derive instance Eq Snake

newtype Maze = Maze (Grid MazeItem)

derive instance Eq Maze

derive instance Newtype Maze _

derive instance Generic Maze _

instance Show Maze where
  show = genericShow

data MazeItem = Maze_Wall | Maze_Floor

derive instance Eq MazeItem

derive instance Generic MazeItem _

instance Show MazeItem where
  show = genericShow

newtype Goodie = Goodie Vec

derive instance Eq Goodie

derive instance Generic Goodie _

instance Show Goodie where
  show = genericShow

data LevelSpec = LevelSpec Snake Maze Direction

---

type Size = Vec

-- f :: forall m. Env m -> m (Array Goodie)
-- f = unfold

-- getFreeBoards :: Size -> Board -> Array Vec
-- getFreeBoards size = positionsInSize size <#> Arr.filter (\x -> )

-- oneOf :: Env m -> NonEmptyArray a -> m a
-- oneOf = unsafeCoerce 1

parseLevelSpecFromBoard :: Board -> Either String LevelSpec
parseLevelSpecFromBoard board = note "E7" ado
  snake <- findSnake board
  let maze = boardToMaze board
  direction <- findSnakeDirection board
  in LevelSpec snake maze direction

parseLevelSpec :: String -> Either String LevelSpec
parseLevelSpec = parseBoard >=> parseLevelSpecFromBoard

-- positionsInSize :: Vec -> Array Vec
-- positionsInSize (Vec sizex sizey) = do
--   x <- Arr.range 0 (sizex - 1)
--   y <- Arr.range 0 (sizey - 1)
--   pure $ Vec x y

mazeItemToTile :: MazeItem -> Tile
mazeItemToTile = case _ of
  Maze_Wall -> Tile_Wall
  Maze_Floor -> Tile_Floor

findFreeSpots :: Board -> Array Vec
findFreeSpots = un Board
  >>> Grid.toMap
  >>> Map.toUnfoldable
  >>> Arr.mapMaybe (\(Tuple k v) -> if v == Tile_Floor then Just k else Nothing)

mkBoard :: Maze -> Snake -> Goodie -> Maybe Board
mkBoard (Maze maze) (Snake snakeHead snakeTail) (Goodie goodie) =
  maze
    <#> mazeItemToTile
    # (\grid -> Grid.insert snakeHead Tile_SnakeHead grid)
    >>= (\grid -> foldM (\g v -> Grid.insert v Tile_SnakeBody g) grid snakeTail)
    >>= (\grid -> Grid.insert goodie Tile_Goodie grid)
    <#> Board

mazeToBoard :: Maze -> Board
mazeToBoard = un Maze >>> map mazeItemToTile >>> Board

applySnake :: Snake -> Board -> Maybe Board
applySnake (Snake snakeHead snakeTail) (Board board) = board
  # (\grid -> Grid.insert snakeHead Tile_SnakeHead grid)
  >>= (\grid -> foldM (\g v -> Grid.insert v Tile_SnakeBody g) grid snakeTail)
  <#> Board

applyGoodie :: Goodie -> Board -> Maybe Board
applyGoodie (Goodie goodie) (Board board) = board
  # (\grid -> Grid.insert goodie Tile_Goodie grid)
  <#> Board

findSnake :: Board -> Maybe Snake
findSnake (Board grid) = ado
  snakeHead <- Grid.findIndex (_ == Tile_SnakeHead) grid
  let snakeTail = unfoldr next snakeHead
  in Snake snakeHead snakeTail
  where
  next :: Vec -> Maybe (Tuple Vec Vec)
  next vec = ado
    nextVec <- (Dir.toVector <$> Dir.directionsClockwise) #
      Arr.find \dirVec -> Grid.lookup (vec + dirVec) grid == Just Tile_SnakeBody
    in Tuple nextVec nextVec

findSnakeDirection :: Board -> Maybe Direction
findSnakeDirection (Board grid) = do
  snakeHead <- Grid.findIndex (_ == Tile_SnakeHead) grid
  dir <- Dir.directionsClockwise #
    Arr.find \dir -> Grid.lookup (snakeHead + Dir.toVector dir) grid == Just Tile_SnakeBody
  pure $ dir

boardToMaze :: Board -> Maze
boardToMaze = un Board
  >>> map case _ of
    Tile_Wall -> Maze_Wall
    _ -> Maze_Floor
  >>> Maze

parseChar :: Char -> Either String Tile
parseChar = case _ of
  '#' -> Right Tile_Wall
  'O' -> Right Tile_SnakeBody
  '+' -> Right Tile_SnakeHead
  ' ' -> Right Tile_Floor
  c -> Left $ "Invalid char" <> show c

parseBoard :: String -> Either String Board
parseBoard str = do
  charBoard <- note "E1" $ CharGrid.fromString str
  board <- traverse parseChar charBoard
  pure $ Board board

printTile :: Tile -> Char
printTile = case _ of
  Tile_Wall -> '#'
  Tile_SnakeBody -> 'O'
  Tile_SnakeHead -> '+'
  Tile_Goodie -> 'x'
  Tile_Floor -> ' '

printBoard :: Board -> String
printBoard = un Board >>> map printTile >>> CharGrid.toString