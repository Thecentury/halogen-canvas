module App.CanvasWorld where

import Prelude

import App.MouseEvent (offsetX, offsetY) as Mouse
import CSS (border, px, solid)
import CSS.Geometry (height, width) as CSS
import Color (black)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.Array.ST as STArray
import Data.Int (rem)
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2)
import Effect (Effect)
import Effect (foreachE) as Effect
import Effect.Aff (Milliseconds(..))
import Effect.Aff (delay, forkAff) as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
--import Effect.Console as Console
import Graphics.Canvas (Context2D)
import Graphics.Canvas as GCanvas
import Halogen as H
import Halogen.Canvas (Input)
import Halogen.Canvas as Canvas
import Halogen.Canvas.Renderer (Renderer)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (altKey) as Mouse

data Cell =
  Empty
  | Concrete

type State = {
  cells :: Array Cell
}

type Coord = { x :: Int, y :: Int }

type WithCoord a = { coord :: Coord, cell :: a }

attachCoord :: forall a . Coord -> a -> WithCoord a
attachCoord coord cell = { coord, cell }

data Generation a =
    Current a
  | Next a

promoteGeneration :: forall a . Generation a -> Generation a
promoteGeneration (Current a) = Next a
promoteGeneration (Next a) = Next a

withoutGeneration :: forall a . Generation a -> a
withoutGeneration (Current a) = a
withoutGeneration (Next a) = a

data Neighbour =
    TopLeft
  | Top
  | TopRight
  | Left
  | Right
  | BottomLeft
  | Bottom
  | BottomRight

neighbourCoord :: Coord -> Neighbour -> Coord
neighbourCoord { x, y } = case _ of
  TopLeft -> { x: x - 1, y: y - 1 }
  Top -> { x, y: y - 1 }
  TopRight -> { x: x + 1, y: y - 1 }
  Left -> { x: x - 1, y }
  Right -> { x: x + 1, y }
  BottomLeft -> { x: x - 1, y: y + 1 }
  Bottom -> { x, y: y + 1 }
  BottomRight -> { x: x + 1, y: y + 1 }

neighbourMut :: forall h a . STArray h a -> Coord -> Neighbour -> ST h (Maybe a)
neighbourMut cells coord n = do
  let coord' = neighbourCoord coord n
  peek coord' cells

coordIndex :: Coord -> Int
coordIndex { x, y } = y * worldWidth + x

indexToCoord :: Int -> Coord
indexToCoord i = { x: i `rem` worldWidth, y: i / worldWidth }

cellsWithCoordinates :: forall a . Array a -> Array (WithCoord a)
cellsWithCoordinates cells =
  Array.mapWithIndex (\i cell -> attachCoord (indexToCoord i) cell) cells

cellAt :: Coord -> Array Cell -> Maybe Cell
cellAt coord cells = Array.index cells (coordIndex coord)

setCell :: Coord -> Cell -> State -> State
setCell coord cell world = world { cells = fromMaybe world.cells $ Array.updateAt (coordIndex coord) cell world.cells }

mousePosToCoord :: MouseEvent -> Coord
mousePosToCoord e =
  let
    x = Mouse.offsetX e / pixelSize
    y = Mouse.offsetY e / pixelSize
  in
    { x, y }

pixelWidth :: Number
pixelWidth = 300.0

pixelHeight :: Number
pixelHeight = 300.0

pixelSize :: Int
pixelSize = 5

worldWidth :: Int
worldWidth = Int.round pixelWidth / pixelSize

worldHeight :: Int
worldHeight = Int.round pixelHeight / pixelSize

data Action =
    Initialize
  | MouseMove MouseEvent
  | Tick

_canvas = Proxy :: Proxy "canvas"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ ->
      { cells: Array.replicate (worldWidth * worldHeight) Empty },
      render,
      eval: H.mkEval H.defaultEval
       { handleAction = handleAction,
         initialize = Just Initialize }
    }

type Slots = ( canvas :: forall query. H.Slot query Void Unit )

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [
      HH.div
        [
          HE.onMouseMove MouseMove,
          style $ do
            CSS.width (px pixelWidth)
            CSS.height (px pixelHeight)
        ]
        [
          HH.slot_ _canvas unit (Canvas.mkComponent cfg) input
        ]
    ]
  where
    cfg = { renderer }
    input :: Input State
    input =
      { picture : state
      , css : Just (border solid (px 0.5) black)
      , size : vec2 pixelWidth pixelHeight
      }

data Picture
  = Rect { x :: Number, y :: Number, width :: Number, height :: Number }
  | Circle { x :: Number, y :: Number, radius :: Number }

coordToRect :: Coord -> GCanvas.Rectangle
coordToRect { x, y } =
  { x: Int.toNumber x * size,
    y: Int.toNumber y * size,
    width: size,
    height: size
  }
  where
    size = Int.toNumber pixelSize


renderer :: Renderer GCanvas.Context2D State
renderer =
  { init, render : renderWorld, onResize }
  where
    init :: Vec D2 Number -> HTMLCanvasElement -> Effect (Maybe Context2D)
    init _size canvasElem =
      map Just $ GCanvas.getContext2D $ unsafeCoerce canvasElem

    renderWorld :: Context2D -> State -> Effect Unit
    renderWorld ctx world = do
      let allCanvas = { x: 0.0, y: 0.0, width: pixelWidth, height: pixelHeight }
      GCanvas.clearRect ctx allCanvas
      Effect.foreachE (cellsWithCoordinates world.cells) $ renderCell ctx

    renderCell :: Context2D -> WithCoord Cell -> Effect Unit
    renderCell ctx { coord, cell } = case cell of
      Empty -> pure unit

      Concrete -> do
        let rect = coordToRect coord
        GCanvas.setFillStyle ctx "#000"
        GCanvas.fillRect ctx rect

    onResize :: Vec D2 Number -> Context2D -> Effect Context2D
    onResize _size ctx =
      pure ctx

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction Initialize = do
    _ <- H.subscribe =<< timer Tick
    pure unit

handleAction (MouseMove e) = do
  if Mouse.altKey e then do
    let coord = mousePosToCoord e
    H.modify_ \state -> setCell coord Concrete state
  else
    pure unit

handleAction Tick =
  H.modify_ \state -> state { cells = updateWorld state.cells }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 100.0
    H.liftEffect $ HS.notify listener val
  pure emitter

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

exchangeF :: forall h a . Coord -> Neighbour -> (a -> a) -> STArray h a -> ST h Unit
exchangeF thisCoord n f cells = do
  let nCoord = neighbourCoord thisCoord n
  thisCell <- peek thisCoord cells
  otherCell <- peek nCoord cells
  case Tuple thisCell otherCell of
    Tuple (Just this) (Just other) -> do
      set nCoord (f this) cells
      set thisCoord (f other) cells
    _ -> pure unit

updateCell :: forall h . WithCoord (Generation Cell) -> STArray h (Generation Cell) -> ST h Unit
updateCell { coord, cell } cells = do
  case cell of
    Current Empty -> pure unit
    Current Concrete -> do
      bottom <- neighbourMut cells coord Bottom
      case withoutGeneration <$> bottom of
        Just Empty ->
          exchangeF coord Bottom promoteGeneration cells
        Just Concrete -> do
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

-- | This type provides a slightly easier way of iterating over an array's
-- | elements in an STArray computation, without having to keep track of
-- | indices.
data Iterator r a = Iterator (Int -> ST r (Maybe a)) (STRef r Int)

-- | Make an Iterator given an indexing function into an array (or anything
-- | else). If `xs :: Array a`, the standard way to create an iterator over
-- | `xs` is to use `iterator (xs !! _)`, where `(!!)` comes from `Data.Array`.
iterator :: forall r a. (Int -> ST r (Maybe a)) -> ST r (Iterator r a)
iterator f = Iterator f <$> STRef.new 0

iteratorAt :: forall r a. Int -> (Int -> ST r (Maybe a)) -> ST r (Iterator r a)
iteratorAt i f = Iterator f <$> STRef.new i

iterateWithIndex :: forall r a. Iterator r a -> (Int -> a -> ST r Unit) -> ST r Unit
iterateWithIndex iter f = do
  break <- STRef.new false
  ST.while (not <$> STRef.read break) do
    Tuple index mx <- nextWithIndex iter
    case mx of
      Just x -> f index x
      Nothing -> void $ STRef.write true break

iterateReverseWithIndex :: forall r a. Iterator r a -> (Int -> a -> ST r Unit) -> ST r Unit
iterateReverseWithIndex iter f = do
  break <- STRef.new false
  ST.while (not <$> STRef.read break) do
    Tuple index mx <- prevWithIndex iter
    case mx of
      Just x -> f index x
      Nothing -> void $ STRef.write true break

nextWithIndex :: forall r a. Iterator r a -> ST r (Tuple Int (Maybe a))
nextWithIndex (Iterator f currentIndex) = do
  i <- STRef.read currentIndex
  _ <- STRef.modify (_ + 1) currentIndex
  element <- f i
  pure $ Tuple i element

prevWithIndex :: forall r a. Iterator r a -> ST r (Tuple Int (Maybe a))
prevWithIndex (Iterator f currentIndex) = do
  i <- STRef.read currentIndex
  _ <- STRef.modify (_ - 1) currentIndex
  element <- f i
  pure $ Tuple i element

updateWorld :: Array Cell -> Array Cell
updateWorld cells = withoutGeneration <$> STArray.run do
  let genCells = Current <$> cells
  cellsMut <- STArray.thaw genCells
  i <- iteratorAt (Array.length genCells - 1) \ix -> STArray.peek ix cellsMut
  iterateReverseWithIndex i \ix cell -> do
    let coord = indexToCoord ix
    updateCell (attachCoord coord cell) cellsMut
  pure cellsMut