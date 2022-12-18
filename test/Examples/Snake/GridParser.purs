module Test.Examples.Snake.GridParser
  ( GridParseError(..)
  , GridParser
  , runParser
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Test.Examples.Snake.Data.Grid (Grid)
import Unsafe.Coerce (unsafeCoerce)


type Context a = Grid (Maybe a)

newtype GridParser a b = GridParser (Context a -> Either String (Tuple (Context a) b))

type GridPrinter a b = b -> Grid a -> Grid a

instance Functor (GridParser a) where
  map f (GridParser g) = GridParser \ctx -> case g ctx of
    Left err -> Left err
    Right res -> Right $ map f res

instance Apply (GridParser a) where
  apply (GridParser h) (GridParser g) = GridParser \ctx ->
    case h ctx of
      Left err -> Left err
      Right (Tuple ctx' f) -> case g ctx' of
        Left err -> Left err
        Right (Tuple ctx'' x) -> Right $ Tuple ctx'' (f x)

instance Applicative (GridParser a) where
  pure x = GridParser \ctx -> Right (Tuple ctx x)


runParser :: forall a b. Grid a -> GridParser a b -> Either GridParseError b
runParser grid (GridParser f) = Left GridParseError


data GridParseError = GridParseError


derive instance Eq GridParseError

derive instance Generic GridParseError _

instance Show GridParseError where
  show = genericShow
