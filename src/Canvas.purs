module Halogen.Canvas
  ( mkComponent
  , Input
  , Vec2n
  )
where

-- https://github.com/thought2/purescript-halogen-canvas/tree/master

import Prelude

import CSS (CSS)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Maybe.Trans as MaybeT
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for)
import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec, (!!))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Canvas.Renderer (Renderer)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLCanvasElement (HTMLCanvasElement)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLElement (HTMLElement)

-- CONSTANTS

refLabel :: H.RefLabel
refLabel = H.RefLabel "canvas"

-- COMPONENT

type Vec2n = Vec D2 Number

type Config ctx picture =
  { renderer :: Renderer ctx picture
  }

type State ctx picture =
  { input :: Input picture
  , ctx :: Maybe ctx
  }

type Input picture =
  { picture :: picture
  , css :: Maybe CSS
  , size :: Vec2n
  }

data Action picture
  = Init
  | HandleInput (Input picture)

mkComponent
  :: forall ctx picture query output m
   . MonadEffect m
  => Config ctx picture
  -> H.Component query (Input picture) output m
mkComponent cfg = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction cfg
      , receive = Just <<< HandleInput
      , initialize = Just Init
      }
  }

-- COMPONENT INIT

initialState :: forall ctx picture . Input picture -> State ctx picture
initialState input =
  { input
  , ctx : Nothing
  }

-- COMPONENT RENDER

type HTML = forall action slots. HH.HTML action slots

render :: forall ctx picture. State ctx picture -> HTML
render { input : { size, css } } =
  HH.canvas $
    [ HP.ref refLabel
    , HP.width $ sizeInt!!d0
    , HP.height $ sizeInt!!d1
    ] <>
    ( maybe mempty (HC.style >>> pure) css
    )
    where
      sizeInt = size <#> Int.floor

-- COMPONENT ACTION

handleAction
  :: forall ctx picture output m
   . MonadEffect m
  => Config ctx picture
  -> Action picture
  -> H.HalogenM (State ctx picture) (Action picture) () output m Unit
handleAction cfg@{renderer} = case _ of
  Init -> map (const unit) $ runMaybeT do
    { input } <- H.get

    htmlElem :: HTMLElement
      <- MaybeT $ H.getHTMLElementRef refLabel

    canvasElem :: HTMLCanvasElement
      <- MaybeT $ pure $ HTMLCanvasElement.fromHTMLElement htmlElem

    ctx :: ctx
      <- MaybeT $ H.liftEffect $ renderer.init input.size canvasElem

    H.modify_ _ { ctx = Just ctx }
    MaybeT.lift $ handleAction cfg (HandleInput input)
    pure unit

  HandleInput input -> do
    state <- H.get
    H.modify_ _ { input = input }

    _ <- for state.ctx \ctx -> do
      when (state.input.size /= input.size) do
        ctx' <- H.liftEffect $ renderer.onResize input.size ctx
        H.modify_ _ { ctx = Just ctx' }

      H.liftEffect $ renderer.render ctx input.picture

    pure unit