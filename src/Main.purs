module Main where

import Prelude

import App.CanvasWorld as CanvasWorld
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI CanvasWorld.component unit body