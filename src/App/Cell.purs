module App.Cell (
  Cell(..),
  AcidizedData,
  updateWorld
) where

import Prelude
import App.Coordinates (Coord, Neighboring, Neighbour(..), WithCoord, attachCoord, attachNeighbour, exchangeF, indexToCoord, neighbourWithCoordMut, set)
import App.MutableArray (iterateReverseWithIndex, iteratorAt, peekWithIndex)
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Data.Traversable (traverse)

type AcidizedData = { was :: Cell, ttl :: Int }

data Cell =
    Empty
  | Acid
  | Acidized AcidizedData
  | Concrete
  | FrozenConcrete

derive instance eqCell :: Eq Cell

data Generation a =
    Current a
  | Next a

promoteGeneration :: forall a . Generation a -> Generation a
promoteGeneration (Current a) = Next a
promoteGeneration (Next a) = Next a

withoutGeneration :: forall a . Generation a -> a
withoutGeneration (Current a) = a
withoutGeneration (Next a) = a

applyToFirstMatching :: forall h . Coord -> Array Neighbour -> (Cell -> Boolean) -> STArray h (Generation Cell) -> (Neighbour -> Coord -> Cell -> ST h Unit) -> ST h Boolean
applyToFirstMatching here neighbours predicate cells run = do
  candidates <- traverse (\n -> neighbourWithCoordMut cells here n <#> map (attachNeighbour n)) neighbours
  let firstMatch =
        candidates
        # Array.mapMaybe identity
        # Array.filter (\{ value : Tuple _ cell } -> predicate $ withoutGeneration cell)
        # Array.head
  result <- traverse run' firstMatch
  case result of
    Just _ -> pure true
    Nothing -> pure false
  where
    run' :: Neighboring (Tuple Coord (Generation Cell)) -> ST h Unit
    run' { neighbour, value : Tuple coord cell } = run neighbour coord $ withoutGeneration cell

updateCell :: forall h . WithCoord (Generation Cell) -> STArray h (Generation Cell) -> ST h Unit
updateCell { coord : here, cell } cells = do
  case cell of
    Current Empty -> pure unit
    Current FrozenConcrete -> pure unit
    Current (Acidized { ttl, was }) -> do
      if ttl == 0 then
        set here (Next Acid) cells
      else
        set here (Next $ Acidized { was: was, ttl: ttl - 1 }) cells
    Current Acid -> do
      bottom <- neighbourWithCoordMut cells here Bottom
      case (\(Tuple coord' c) -> Tuple coord' (withoutGeneration c)) <$> bottom of
        Just (Tuple _ Empty) ->
          exchangeF here Bottom promoteGeneration cells
        Just (Tuple _ prevCell) -> do
          _ <- applyToFirstMatching here [Bottom, BottomLeft, BottomRight] (flip Array.elem [Concrete, Empty]) cells
                (\n there nCell -> do
                  if nCell == Concrete then do
                    set there (Next $ Acidized { was: prevCell, ttl: 10 }) cells
                    set here (Next Empty) cells
                  else
                    exchangeF here n promoteGeneration cells)
          pure unit
        Nothing -> pure unit

    Current Concrete -> do
      _ <- applyToFirstMatching
            here
            [Bottom, BottomLeft, BottomRight]
            (\n -> n == Empty || n == Acid)
            cells
            (\n _ _ -> exchangeF here n promoteGeneration cells)
      pure unit
    -- Do not update already updated cells.
    Next _ -> pure unit

updateWorld :: Array Cell -> Array Cell
updateWorld cells = withoutGeneration <$> STArray.run do
  let genCells = Current <$> cells
  cellsMut <- STArray.thaw genCells
  i <- iteratorAt (Array.length genCells - 1) $ peekWithIndex cellsMut
  iterateReverseWithIndex i \(Tuple ix cell) -> do
    let coord = indexToCoord ix
    updateCell (attachCoord coord cell) cellsMut
  pure cellsMut