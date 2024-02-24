module Main where

import Prelude

import App.CanvasWorldUI as CanvasWorldUI
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI CanvasWorldUI.component unit body