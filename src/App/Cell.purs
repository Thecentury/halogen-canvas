module App.Cell (
  Cell(..),
  AcidData,
  AcidizedData,
  updateWorld
) where

import Prelude
import App.Coordinates (Coord, Neighboring, Neighbour(..), WithCoord, attachCoord, attachNeighbour, coordIndex, exchangeF, indexToCoord, neighbourWithCoordMut, set)
import App.MutableArray (iterateReverseWithIndex, iteratorAt, peekWithIndex)
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Data.Traversable (traverse)

{----------------------------------------------------------------------------------------------------------------------}

data Generation a =
    Current a
  | Next a

derive instance eqGeneration :: Eq a => Eq (Generation a)
derive instance functorGeneration :: Functor Generation

promoteGeneration :: forall a . Generation a -> Generation a
promoteGeneration (Current a) = Next a
promoteGeneration (Next a) = Next a

withoutGeneration :: forall a . Generation a -> a
withoutGeneration (Current a) = a
withoutGeneration (Next a) = a

mapPreservingGeneration :: forall h a . Coord -> (a -> a) -> STArray h (Generation a) -> ST h Unit
mapPreservingGeneration coord f cells = do
  let i = coordIndex coord
  _ <- STArray.modify i (map f) cells
  pure unit

{----------------------------------------------------------------------------------------------------------------------}

type AcidData = { horizontalForce :: Int }

type AcidizedData = { was :: Cell, ttl :: Int }

data Cell =
    Empty
  | Acid AcidData
  | Acidized AcidizedData
  | Concrete
  | FrozenConcrete

derive instance eqCell :: Eq Cell

isAcid :: Cell -> Boolean
isAcid (Acid _) = true
isAcid _ = false

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
        set here (Next $ Acid { horizontalForce: 0 }) cells
      else
        set here (Next $ Acidized { was: was, ttl: ttl - 1 }) cells
    Current (Acid { horizontalForce })  -> do
      -- Try acidizing concrete on the bottom or moving to an empty cell.
      acidizedOrDropped <- applyToFirstMatching here [Bottom, BottomLeft, BottomRight] (flip Array.elem [Concrete, Empty]) cells
        (\n there nCell -> do
          case nCell of
            Concrete -> do
              set there (Next $ Acidized { was: nCell, ttl: 10 }) cells
              set here (Next Empty) cells
            Empty ->
              exchangeF here n promoteGeneration cells
            _ -> pure unit)
      if not acidizedOrDropped then do
        -- If cannot fall or acidize, try to apply horizontal pressure to the bottom diagonal cells.
        _ <- applyToFirstMatching here [BottomLeft] isAcid cells $ \_ there nCell -> do
          case nCell of
            Acid nAcid ->
              mapPreservingGeneration there (const $ Acid { horizontalForce: nAcid.horizontalForce - 1 }) cells
            _ -> pure unit
        _ <- applyToFirstMatching here [BottomRight] isAcid cells $ \_ there nCell -> do
          case nCell of
            Acid nAcid ->
              mapPreservingGeneration there (const $ Acid { horizontalForce: nAcid.horizontalForce + 1 }) cells
            _ -> pure unit
        if horizontalForce < 0 then do
          left <- neighbourWithCoordMut cells here Left
          case (map withoutGeneration) <$> left of
            Just (Tuple there Empty) -> do
              set there (Next $ Acid { horizontalForce: 0 }) cells
              set here (Next Empty) cells
            Just (Tuple there (Acid { horizontalForce: thereForce })) ->
              mapPreservingGeneration there (const $ Acid { horizontalForce: thereForce - 1 }) cells
            _ -> pure unit
        else if horizontalForce > 0 then do
          right <- neighbourWithCoordMut cells here Right
          case (map withoutGeneration) <$> right of
            Just (Tuple there Empty) -> do
              set there (Next $ Acid { horizontalForce: 0 }) cells
              set here (Next Empty) cells
            Just (Tuple there (Acid { horizontalForce: thereForce })) ->
              mapPreservingGeneration there (const $ Acid { horizontalForce: thereForce + 1 }) cells
            _ -> pure unit
          pure unit
        else pure unit
      else
        pure unit

    Current Concrete -> do
      _ <- applyToFirstMatching
            here
            [Bottom, BottomLeft, BottomRight]
            (\n -> n == Empty || isAcid n)
            cells
            (\n _ _ -> exchangeF here n promoteGeneration cells)
      pure unit
    -- Do not update already updated cells.
    Next _ -> pure unit

updateWorld' :: Array (Generation Cell) -> Array (Generation Cell)
updateWorld' cells = STArray.run do
  cellsMut <- STArray.thaw cells
  i <- iteratorAt (Array.length cells - 1) $ peekWithIndex cellsMut
  iterateReverseWithIndex i \(Tuple ix cell) -> do
   let coord = indexToCoord ix
   updateCell (attachCoord coord cell) cellsMut
  pure cellsMut

type CurrNextCount = { current :: Int, next :: Int }

updateWorld :: Array Cell -> Tuple Int (Array Cell)
updateWorld cells =
  let (Tuple iteration cells') = iterate 0 { current: 0, next: 0 } (Current <$> cells)
  in Tuple iteration (withoutGeneration <$> cells')
  where
  iterate :: Int -> CurrNextCount -> Array (Generation Cell) -> Tuple Int (Array (Generation Cell))
  iterate iteration stats c =
    let
     cells' = updateWorld' c
     newStats = Array.foldl (\{ current, next } cell ->
      case cell of
        Current _ -> { current: current + 1, next }
        Next _ -> { current, next: next + 1 }
      ) { current: 0, next: 0 } cells'
   in
     if newStats == stats then
        Tuple iteration cells'
      else
        iterate (iteration + 1) newStats cells'