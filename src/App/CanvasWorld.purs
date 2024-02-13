module App.CanvasWorld where

import Prelude

import Color (black)
import CSS (border, px, solid)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Graphics.Canvas (Context2D)
import Graphics.Canvas as GCanvas
import Halogen as H
import Halogen.Canvas (Input)
import Halogen.Canvas as Canvas
import Halogen.Canvas.Renderer (Renderer)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (altKey) as Mouse
import App.MouseEvent (offsetX, offsetY) as Mouse
import CSS.Geometry (height, width) as CSS
import Halogen.HTML.CSS (style)
import Data.Int (rem)

data Cell =
  Empty
  | Concrete

type State = {
  cells :: Array Cell
}

type Coord = { x :: Int, y :: Int }

type WithCoord a = { coord :: Coord, cell :: a }

coordIndex :: Coord -> Int
coordIndex { x, y } = y * worldWidth + x

indexToCoord :: Int -> Coord
indexToCoord i = { x: i `rem` worldWidth, y: i / worldWidth }

cellsWithCoordinates :: State -> Array (WithCoord Cell)
cellsWithCoordinates world =
  Array.zipWith (\i cell -> { coord: indexToCoord i, cell }) (Array.range 0 (Array.length world.cells)) world.cells

cellAt :: Coord -> State -> Maybe Cell
cellAt coord world = Array.index world.cells (coordIndex coord)

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
pixelSize = 3

worldWidth :: Int
worldWidth = Int.round pixelWidth / pixelSize

worldHeight :: Int
worldHeight = Int.round pixelHeight / pixelSize

data Action =
  MouseMove MouseEvent

_canvas = Proxy :: Proxy "canvas"

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ ->
      { cells: Array.replicate (worldWidth * worldHeight) Empty },
      render,
      eval: H.mkEval H.defaultEval { handleAction = handleAction }
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
    renderWorld ctx world =
      traverse_ (renderCell ctx) $ cellsWithCoordinates world

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

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction (MouseMove e) = do
  if Mouse.altKey e then
    let coord = mousePosToCoord e in
    H.modify_ \state -> setCell coord Concrete state
  else
    pure unit