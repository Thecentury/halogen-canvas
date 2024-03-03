module App.CanvasWorldUI where

import Prelude

import App.Cell (Cell(..), updateWorld)
import App.Coordinates (Coord, WithCoord, cellsWithCoordinates, pointsBetweenCoordinates, coordIndex, heightInPixels, pixelSize, widthInPixels, worldHeight, worldWidth)
import App.MouseEvent (offsetX, offsetY) as Mouse
import CSS (border, px, solid)
import CSS.Geometry (height, width) as CSS
import Color (black)
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Int (rem)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2)
import DOM.HTML.Indexed.InputType (InputType(..))
import Effect (Effect)
import Effect (foreachE) as Effect
import Effect.Aff (Milliseconds(..))
import Effect.Aff (delay, forkAff) as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Graphics.Canvas (Context2D)
import Graphics.Canvas as GCanvas
import Halogen as H
import Halogen.Canvas (Input)
import Halogen.Canvas as Canvas
import Halogen.Canvas.Renderer (Renderer)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (altKey, buttons) as Mouse
import Data.Tuple (Tuple(Tuple))
import Web.HTML.Common (ClassName(..))
import Data.Foldable (foldl)
import Web.UIEvent.KeyboardEvent (altKey) as Keyboard
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

type MaterialSelectorSpec = {
  material :: Cell,
  name :: String
}

materialSelectors :: Array MaterialSelectorSpec
materialSelectors = [
  { material: Concrete, name: "Concrete" },
  { material: Acid { horizontalForce: 0 }, name: "Acid" },
  { material: Empty, name: "Empty" }
]

type State = {
  cells :: Array Cell,
  activeMaterial :: Cell,
  previousMousePos :: Maybe Coord
}

setCell :: Cell -> Array Cell -> Coord -> Array Cell
setCell cell cells coord = fromMaybe cells $ Array.updateAt (coordIndex coord) cell cells

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
  | ClearPreviousMousePosition
  | KeyUp KeyboardEvent
  | ActiveMaterialChanged Cell
  | Tick

_canvas = Proxy :: Proxy "canvas"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ ->
      {
        cells: Array.replicate (worldWidth * worldHeight) Empty,
        activeMaterial: Concrete,
        previousMousePos: Nothing
      },
      render,
      eval: H.mkEval H.defaultEval
       { handleAction = handleAction,
         initialize = Just Initialize }
    }

type Slots = ( canvas :: forall query. H.Slot query Void Unit )

renderMaterialSelector :: forall m. MonadEffect m => State -> MaterialSelectorSpec -> H.ComponentHTML Action Slots m
renderMaterialSelector { activeMaterial } { material, name } =
  HH.div
    [ HP.class_ $ ClassName "material-selector-item" ]
    [
      HH.input
        [ HP.type_ InputRadio,
          HP.checked $ material == activeMaterial,
          HE.onClick (\_ -> ActiveMaterialChanged material),
          HP.id name
        ],
      HH.label [
        HP.for name
      ] [ HH.text name ]
    ]

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ $ ClassName "main-layout" ]
    [
      HH.div
        [
          -- To enable firing of keyboard events. It doesn't work reliably though, as the canvas still has to be focused.
          HP.tabIndex 0,
          HE.onMouseMove MouseMove,
          HE.onMouseUp $ const ClearPreviousMousePosition,
          HE.onKeyUp KeyUp,
          HP.class_ $ ClassName "canvas",
          style $ do
            CSS.width (px widthInPixels)
            CSS.height (px heightInPixels)
        ]
        [
          HH.slot_ _canvas unit (Canvas.mkComponent cfg) input
        ],
      HH.div [
        HP.class_ $ ClassName "material-selector"
      ] [
        HH.fieldset [] $
          [ HH.legend [] [ HH.text "Select a material:" ] ] <>
          materialSelectorsHtml
      ]
    ]
  where
    cfg = { renderer }
    input :: Input State
    input =
      { picture : state,
        css : Just (border solid (px 0.5) black),
        size : vec2 widthInPixels heightInPixels }
    materialSelectorsHtml = renderMaterialSelector state <$> materialSelectors

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
      Acidized { ttl } | ttl `rem` 2 == 0 ->
        renderCell ctx { coord, cell: Acid { horizontalForce: 0 } }
      Acidized { was } ->
        renderCell ctx { coord, cell: was }
      Acid { horizontalForce: 0 } -> coloredRect "#7FFF00"
      Acid { horizontalForce } | horizontalForce < 0 -> coloredRect "#c9fc95"
      Acid _ -> coloredRect "#4b9400"
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
  if Mouse.altKey e || Mouse.buttons e == 1 then do
    H.modify_ \state ->
      let
        mousePos = mousePosToCoord e
        previousMousePos = fromMaybe mousePos state.previousMousePos
        intermediatePoints = pointsBetweenCoordinates previousMousePos mousePos
        intermediatePoints' =
          if intermediatePoints == [] then
            [mousePos]
          else
           intermediatePoints
        updatedCells = foldl (setCell state.activeMaterial) state.cells intermediatePoints'
      in
      state {
        previousMousePos = Just mousePos,
        cells = updatedCells
      }
  else
    pure unit

handleAction ClearPreviousMousePosition = do
  H.modify_ \state -> state { previousMousePos = Nothing }

handleAction (KeyUp e) = do
  if Keyboard.altKey e then do
    H.modify_ \state -> state { previousMousePos = Nothing }
    liftEffect $ Console.log "Alt key up"
  else
    pure unit

handleAction Tick = do
  cells <- H.gets (_.cells)
  let (Tuple iterations cells') = updateWorld cells
  H.modify_ \state -> state { cells = cells' }
  liftEffect $ Console.log $ "Iterations: " <> show iterations

handleAction (ActiveMaterialChanged material) =
  H.modify_ \state -> state { activeMaterial = material }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 100.0
    H.liftEffect $ HS.notify listener val
  pure emitter