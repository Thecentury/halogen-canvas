module App.HTML.Touch where

import Web.TouchEvent.Touch (Touch)

foreign import offsetX :: Touch -> Int

foreign import offsetY :: Touch -> Int