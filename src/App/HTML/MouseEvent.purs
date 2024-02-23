module App.MouseEvent where

import Web.UIEvent.MouseEvent(MouseEvent)

foreign import offsetX :: MouseEvent -> Int

foreign import offsetY :: MouseEvent -> Int