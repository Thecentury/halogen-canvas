module App.Button where

import Prelude

import Color (black)
import CSS (border, px, solid)
import Data.Array as Array
import Data.Int as Int
import Data.List (range)
import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Traversable (traverse_)
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Graphics.Canvas (Context2D)
import Graphics.Canvas as GCanvas
import Halogen as H
import Halogen.Canvas (Input)
import Halogen.Canvas as Canvas
import Halogen.Canvas.Renderer (Renderer)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (clientX, clientY, screenX, screenY) as Mouse
import App.MouseEvent (offsetX, offsetY) as Mouse
import CSS.Geometry (height, width)
import Halogen.HTML.CSS (style)

type State
  = { count :: Int }

data Action =
   Increment
 | MouseMove MouseEvent

_canvas = Proxy :: Proxy "canvas"

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

type Slots = ( canvas :: forall query. H.Slot query Void Unit )

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render _state =
  HH.div_
    [
      HH.div
        [
          HE.onMouseMove MouseMove,
          style $ do
            width (px 300.0)
            height (px 300.0)
        ]
        [
          HH.slot_ _canvas unit (Canvas.mkComponent cfg) input
        ]
    ]
  where
    cfg = { renderer }
    input :: Input (Array Picture)
    input =
      { picture : rectangles
      , css : Just (border solid (px 0.5) black)
      , size : vec2 w h
      }
    -- 100 rectangles moving from left to right:
    rectangles :: Array Picture
    rectangles =
      Array.fromFoldable
      $ (\i -> Rect { x: (Int.toNumber i) * 10.0 + 2.0, y: 10.0, width: 6.0, height: 6.0 })
      <$> range 1 30

    w = 300.0
    h = 300.0

data Picture
  = Rect { x :: Number, y :: Number, width :: Number, height :: Number }
  | Circle { x :: Number, y :: Number, radius :: Number }

renderer :: Renderer GCanvas.Context2D (Array Picture)
renderer =
  { init, render : render2, onResize }
  where
    init :: Vec D2 Number -> HTMLCanvasElement -> Effect (Maybe Context2D)
    init _size canvasElem =
      map Just $ GCanvas.getContext2D $ unsafeCoerce canvasElem

    render2 :: Context2D -> Array Picture -> Effect Unit
    render2 ctx pics =
      traverse_ (renderPicture ctx) pics

    renderPicture :: Context2D -> Picture -> Effect Unit
    renderPicture ctx = case _ of
      Rect opt -> do
        GCanvas.setFillStyle ctx "#000"
        GCanvas.fillRect ctx opt

      Circle { x, y, radius } -> do
        GCanvas.arc ctx {x, y, radius, start : zero, end : pi * 2.0, useCounterClockwise : false }
        GCanvas.stroke ctx

    onResize :: Vec D2 Number -> Context2D -> Effect Context2D
    onResize _size ctx =
      pure ctx

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction Increment = H.modify_ \st -> st { count = st.count + 1 }
handleAction (MouseMove e) = do
  liftEffect $ Console.log msg
  where
    msg = "Mouse moved. Client: "
          <> (show $ Mouse.clientX e)
          <> ", "
          <> (show $ Mouse.clientY e)
          <> ". Screen: "
          <> (show $ Mouse.screenX e)
          <> ", "
          <> (show $ Mouse.screenY e)
          <> ". Offset: "
          <> (show $ Mouse.offsetX e)
          <> ", "
          <> (show $ Mouse.offsetY e)