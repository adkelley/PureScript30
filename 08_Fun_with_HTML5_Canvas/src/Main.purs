module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Graphics.Canvas (CanvasElement, Context2D, LineCap(..), LineJoin(..), getContext2D, setLineCap, setLineJoin, setLineWidth, setStrokeStyle)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element, toEventTarget)
import Web.DOM.ParentNode (querySelector)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import offsetX :: MouseEvent → Int
foreign import offsetY :: MouseEvent → Int

toCanvasElement :: Element → CanvasElement
toCanvasElement = unsafeCoerce

fromCanvasElement :: CanvasElement → Element
fromCanvasElement = unsafeCoerce

toMouseEvent :: Event → MouseEvent
toMouseEvent = unsafeCoerce

draw :: Context2D → Boolean → Int → Int → Int → Effect Unit
draw ctx isDrawing ox oy hue =
  if isDrawing
    then do
      log "I'm drawing"
      log $ show ox
    else do
      log "Im not drawing"


main :: Effect Unit
main = do
  doc ← map toParentNode (window >>= document)
  _canvas ← querySelector (wrap "#draw") doc
  case (map toCanvasElement _canvas) of
    Just ce → do
      ctx ← getContext2D ce
      setStrokeStyle ctx "#BADA55"
      setLineJoin ctx RoundJoin
      setLineCap ctx Round
      setLineWidth ctx 100.0
      let isDrawing = false
      let hue = 0
      el1 ← eventListener \e → draw ctx true (offsetX $ toMouseEvent e) (offsetY $ toMouseEvent e) hue
      el2 ← eventListener \e → draw ctx false (offsetX $ toMouseEvent e) (offsetY $ toMouseEvent e) hue
      case (map toEventTarget _canvas) of
        Just canvas → do
          addEventListener (wrap "mousemove") el1 false canvas
          addEventListener (wrap "mouseup") el2 false canvas
          addEventListener (wrap "mousedown") el1 false canvas
          addEventListener (wrap "mouseout") el2 false canvas
        Nothing → pure unit
    Nothing → throw "no draw element"
