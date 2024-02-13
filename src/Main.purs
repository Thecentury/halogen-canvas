module Main where

import Prelude

import App.CanvasWorld as CanvasWorld
import Effect (Effect)
import Effect as Effect
import Effect.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI CanvasWorld.component unit body