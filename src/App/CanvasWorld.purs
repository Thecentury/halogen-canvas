module App.CanvasWorld where

import Prelude

import App.MouseEvent (offsetX, offsetY) as Mouse
import App.MutableArray (iterateReverseWithIndex, iteratorAt, peekWithIndex)
import CSS (border, px, solid)
import CSS.Geometry (height, width) as CSS
import Color (black)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST (ST)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2)
import Effect (Effect)
import Effect (foreachE) as Effect
import Effect.Aff (Milliseconds(..))
import Effect.Aff (delay, forkAff) as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
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
import Web.UIEvent.MouseEvent (altKey, shiftKey) as Mouse
import App.Coordinates (Coord, Neighbour(..), WithCoord, attachCoord, cellsWithCoordinates, coordIndex, heightInPixels, indexToCoord, neighbourMut, neighbourWithCoordMut, pixelSize, set, widthInPixels, worldHeight, worldWidth, exchangeF)

data Cell =
    Empty
  | Acid
  | Concrete
  | FrozenConcrete

type State = {
  cells :: Array Cell
}

data Generation a =
    Current a
  | Next a

promoteGeneration :: forall a . Generation a -> Generation a
promoteGeneration (Current a) = Next a
promoteGeneration (Next a) = Next a

withoutGeneration :: forall a . Generation a -> a
withoutGeneration (Current a) = a
withoutGeneration (Next a) = a

setCell :: Coord -> Cell -> State -> State
setCell coord cell world = world { cells = fromMaybe world.cells $ Array.updateAt (coordIndex coord) cell world.cells }

mousePosToCoord :: MouseEvent -> Coord
mousePosToCoord e =
  let
    x = Mouse.offsetX e / pixelSize
    y = Mouse.offsetY e / pixelSize
    validX = clamp 0 (worldWidth - 1) x
    validY = clamp 0 (worldHeight - 1) y
  in
    { x: validX, y: validY }

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
            CSS.width (px widthInPixels)
            CSS.height (px heightInPixels)
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
      , size : vec2 widthInPixels heightInPixels
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
      let allCanvas = { x: 0.0, y: 0.0, width: widthInPixels, height: heightInPixels }
      GCanvas.clearRect ctx allCanvas
      Effect.foreachE (cellsWithCoordinates world.cells) $ renderCell ctx

    renderCell :: Context2D -> WithCoord Cell -> Effect Unit
    renderCell ctx { coord, cell } = case cell of
      Empty -> pure unit
      Acid -> coloredRect "#7FFF00"
      FrozenConcrete -> coloredRect "#000"
      Concrete -> coloredRect "#111"

      where
        coloredRect :: String -> Effect Unit
        coloredRect color = do
          let rect = coordToRect coord
          GCanvas.setFillStyle ctx color
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
    spawnCell Concrete
  else if Mouse.shiftKey e then do
    spawnCell Acid
  else
    pure unit
  where
    spawnCell :: Cell -> forall cs o m. MonadAff m => H.HalogenM State Action cs o m Unit
    spawnCell cell = do
      let coord = mousePosToCoord e
      H.modify_ $ setCell coord cell

handleAction Tick =
  H.modify_ \state -> state { cells = updateWorld state.cells }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 100.0
    H.liftEffect $ HS.notify listener val
  pure emitter

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