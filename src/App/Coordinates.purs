-- | Bacic functions for working with coordinates and cells of the world.
module App.Coordinates where

import Prelude
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))

widthInPixels :: Number
widthInPixels = 300.0

heightInPixels :: Number
heightInPixels = 300.0

pixelSize :: Int
pixelSize = 5

worldWidth :: Int
worldWidth = Int.round widthInPixels / pixelSize

worldHeight :: Int
worldHeight = Int.round heightInPixels / pixelSize

type Coord = { x :: Int, y :: Int }

coordIndex :: Coord -> Int
coordIndex { x, y } = y * worldWidth + x

indexToCoord :: Int -> Coord
indexToCoord i = { x: i `Int.rem` worldWidth, y: i / worldWidth }

cellsWithCoordinates :: forall a . Array a -> Array (WithCoord a)
cellsWithCoordinates cells =
  Array.mapWithIndex (\i cell -> attachCoord (indexToCoord i) cell) cells

type WithCoord a = { coord :: Coord, cell :: a }

attachCoord :: forall a . Coord -> a -> WithCoord a
attachCoord coord cell = { coord, cell }

peek :: forall h a
           . Coord
          -> STArray h a
          -> ST h (Maybe a)
peek coord cells = do
  let i = coordIndex coord
  STArray.peek i cells

set :: forall h a
          . Coord
          -> a
          -> STArray h a
          -> ST h Unit
set coord cell cells = do
  let i = coordIndex coord
  _ <- STArray.poke i cell cells
  pure unit

data Neighbour =
    TopLeft
  | Top
  | TopRight
  | Left
  | Right
  | BottomLeft
  | Bottom
  | BottomRight

type Neighboring a = { neighbour :: Neighbour, value :: a }

attachNeighbour :: forall a . Neighbour -> a -> Neighboring a
attachNeighbour neighbour value = { neighbour, value }

neighbourCoord :: Coord -> Neighbour -> Maybe Coord
neighbourCoord { x, y } neighbour = validate $
  case neighbour of
    TopLeft -> { x: x - 1, y: y - 1 }
    Top -> { x, y: y - 1 }
    TopRight -> { x: x + 1, y: y - 1 }
    Left -> { x: x - 1, y }
    Right -> { x: x + 1, y }
    BottomLeft -> { x: x - 1, y: y + 1 }
    Bottom -> { x, y: y + 1 }
    BottomRight -> { x: x + 1, y: y + 1 }
  where
  validate :: Coord -> Maybe Coord
  validate coord =
    if coord.x >= 0 && coord.x < worldWidth && coord.y >= 0 && coord.y < worldHeight then
      Just coord
    else
      Nothing

neighbourMut :: forall h a . STArray h a -> Coord -> Neighbour -> ST h (Maybe a)
neighbourMut cells coord n = do
  let coord' = neighbourCoord coord n
  case coord' of
    Nothing -> pure Nothing
    Just coord'' -> peek coord'' cells

neighbourWithCoordMut :: forall h a . STArray h a -> Coord -> Neighbour -> ST h (Maybe (Tuple Coord a))
neighbourWithCoordMut cells coord n = do
  let coord' = neighbourCoord coord n
  case coord' of
    Nothing -> pure Nothing
    Just coord'' -> do
      neighbourCell <- peek coord'' cells
      pure $ neighbourCell <#> \cell -> Tuple coord'' cell

-- | Exchanges the value of the cell at the given coordinate with the value of the given neighbouring cell.
-- | Applies the function to both cells before exchanging them.
exchangeF :: forall h a . Coord -> Neighbour -> (a -> a) -> STArray h a -> ST h Unit
exchangeF thisCoord n f cells = do
  let maybeNeighbour = neighbourCoord thisCoord n
  case maybeNeighbour of
    Nothing -> pure unit
    Just nCoord -> do
      thisCell <- peek thisCoord cells
      otherCell <- peek nCoord cells
      case Tuple thisCell otherCell of
        Tuple (Just this) (Just other) -> do
          set nCoord (f this) cells
          set thisCoord (f other) cells
        _ -> pure unit