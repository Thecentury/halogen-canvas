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
import Data.Ord (abs, signum)
import Data.Array ((:))
import Data.Number (pow, sqrt)
import Data.Int (toNumber)

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

type Vector = { dx :: Int, dy :: Int }

addVector :: Coord -> Vector -> Coord
addVector { x, y } { dx, dy } = { x: x + dx, y: y + dy }

betweenPoints :: Coord -> Coord -> Vector
betweenPoints { x: x1, y: y1 } { x: x2, y: y2 } = { dx: x2 - x1, dy: y2 - y1 }

signumVector :: Vector -> Vector
signumVector { dx, dy } = { dx: signum dx, dy: signum dy }

dist :: Coord -> Coord -> Number
dist { x: x1, y: y1 } { x: x2, y: y2 } =
  sqrt $ (toNumber (x2 - x1)) `pow` 2.0 + (toNumber (y2 - y1)) `pow` 2.0

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

pointsBetweenCoordinates :: Coord -> Coord -> Array Coord
pointsBetweenCoordinates p1 p2 =
  if p1 == p2 then
    [p1]
  else
    p1 : (pointsBetweenCoordinates nextPoint p2) where

  nextPoint :: Coord
  nextPoint =
    let
      between = betweenPoints p1 p2
      _45deg = abs between.dx == abs between.dy
      v = signumVector between
      next1 = addVector p1 v
      dist1 = dist next1 p2
      next2 = addVector p1 { dx: 0, dy: v.dy }
      dist2 = dist next2 p2
      next3 = addVector p1 { dx: v.dx, dy: 0 }
      dist3 = dist next3 p2
    in
      if _45deg then
        if dist1 < dist2 && dist1 < dist3 then
          next1
        else if dist2 < dist1 && dist2 < dist3 then
          next2
        else
          next3
      else
        if dist2 < dist3 then
          next2
        else
          next3

------------------------------------------------------------------------------------------------------------------------

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
neighbourMut cells center n = do
  let coord' = neighbourCoord center n
  case coord' of
    Nothing -> pure Nothing
    Just coord'' -> peek coord'' cells

neighbourWithCoordMut :: forall h a . STArray h a -> Coord -> Neighbour -> ST h (Maybe (Tuple Coord a))
neighbourWithCoordMut cells center n = do
  let coord' = neighbourCoord center n
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