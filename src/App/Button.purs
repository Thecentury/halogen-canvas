module App.Button where

import Prelude

import Color (white)
import CSS (border, px, solid)
import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Traversable (traverse_)
import Data.Typelevel.Num (D2)
import Data.Vec (Vec, vec2)
import Effect (Effect)
import Effect.Class (class MonadEffect)
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

type State
  = { count :: Int }

data Action
  = Increment

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
render state =
  HH.div_
    [ 
      HH.div_
        [ 
          HH.slot_ _canvas unit (Canvas.mkComponent cfg) input
        ]
    ]
  where
    cfg = { renderer }
    input :: Input (Array Picture)
    input =
      { picture :
          [ Rect
              { x: halve w
              , y : halve h
              , width : halve h * 0.9
              , height : halve h * 0.9
              }
          , Circle
              { x : halve w
              , y : halve h
              , radius : h * 0.49
              }
          ]
      , css : Just (border solid (px 0.0) white)
      , size : vec2 w h
      }
    w = 300.0
    h = 300.0
    halve x = x / 2.0

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
      Rect opt ->
        GCanvas.strokeRect ctx opt

      Circle { x, y, radius } -> do
        GCanvas.arc ctx {x, y, radius, start : zero, end : pi * 2.0, useCounterClockwise : false }
        GCanvas.stroke ctx

    onResize :: Vec D2 Number -> Context2D-> Effect Context2D
    onResize _size ctx =
      pure ctx
      
handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }