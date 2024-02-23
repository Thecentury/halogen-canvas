module Halogen.Canvas.Renderer where

import Prelude

import Data.Maybe (Maybe)
import Data.Typelevel.Num (D2)
import Data.Vec (Vec)
import Effect (Effect)
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)

type Renderer ctx pic =
  { init :: Vec D2 Number -> HTMLCanvasElement -> Effect (Maybe ctx)
  , render :: ctx -> pic -> Effect Unit
  , onResize :: Vec D2 Number -> ctx -> Effect ctx
  }