module App.Cell (
  Cell(..),
  updateWorld
) where

import Prelude
import App.Coordinates (Neighbour(..), WithCoord, attachCoord, exchangeF, indexToCoord, neighbourMut, neighbourWithCoordMut, set)
import App.MutableArray (iterateReverseWithIndex, iteratorAt, peekWithIndex)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))

data Cell =
    Empty
  | Acid
  | Concrete
  | FrozenConcrete

data Generation a =
    Current a
  | Next a

promoteGeneration :: forall a . Generation a -> Generation a
promoteGeneration (Current a) = Next a
promoteGeneration (Next a) = Next a

withoutGeneration :: forall a . Generation a -> a
withoutGeneration (Current a) = a
withoutGeneration (Next a) = a

updateCell :: forall h . WithCoord (Generation Cell) -> STArray h (Generation Cell) -> ST h Unit
updateCell { coord, cell } cells = do
  case cell of
    Current Empty -> pure unit
    Current FrozenConcrete -> pure unit
    Current Acid -> do
      bottom <- neighbourWithCoordMut cells coord Bottom
      case (\(Tuple coord' c) -> Tuple coord' (withoutGeneration c)) <$> bottom of
        Just (Tuple _ Empty) ->
          exchangeF coord Bottom promoteGeneration cells
        Just (Tuple nCoord Concrete) -> do
          set nCoord (Next Acid) cells
          set coord (Next Empty) cells
        Just (Tuple _ Acid) -> pure unit
        Just (Tuple _ FrozenConcrete) -> pure unit
        Nothing -> pure unit

    Current Concrete -> do
      bottom <- neighbourMut cells coord Bottom
      case withoutGeneration <$> bottom of
        Just Empty ->
          exchangeF coord Bottom promoteGeneration cells
        Just _ -> do
          bottomLeft <- neighbourMut cells coord BottomLeft
          case withoutGeneration <$> bottomLeft of
            Just Empty ->
              exchangeF coord BottomLeft promoteGeneration cells
            _ -> do
              bottomRight <- neighbourMut cells coord BottomRight
              case withoutGeneration <$> bottomRight of
                Just Empty ->
                  exchangeF coord BottomRight promoteGeneration cells
                _ -> pure unit
        Nothing -> pure unit
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