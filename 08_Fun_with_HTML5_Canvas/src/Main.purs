module Main where

import Prelude

import Color (toHexString, hsv)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (Ref, new, read, write)
import Graphics.Canvas (CanvasElement, Context2D, LineCap(..), LineJoin(..), beginPath, getContext2D, lineTo, moveTo, setLineCap, setLineJoin, setLineWidth, setStrokeStyle, stroke)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element, toEventTarget)
import Web.DOM.ParentNode (querySelector)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import offsetX :: MouseEvent → Number
foreign import offsetY :: MouseEvent → Number
-- foreign import lineWidth :: Context2D → Number

type AppState = { isDrawing :: Boolean
                , lastX :: Number
                , lastY :: Number
                , hue :: Number
                , direction :: Boolean
                , lineWidth :: Number
                }

fromElement :: Element → CanvasElement
fromElement = unsafeCoerce

toMouseEvent :: Event → MouseEvent
toMouseEvent = unsafeCoerce

draw :: MouseEvent → Context2D → Ref AppState → Number → Number → Effect Unit
draw me ctx _appState ox oy = do
  appState@{isDrawing, lastX, lastY, hue, direction, lineWidth} <- read _appState
  if isDrawing
    then do
      setStrokeStyle ctx (toHexString $ hsv hue 1.0 0.5)
      beginPath ctx
      moveTo ctx lastX lastY
      lineTo ctx ox oy
      stroke ctx
      let direction_ =
            if (lineWidth >= 100.0 || lineWidth <= 1.0)
              then not direction
              else direction
      let lineWidth_ =
            if direction_
              then lineWidth + 1.0
              else lineWidth - 1.0
      write { isDrawing: true
            , lastX: offsetX me
            , lastY: offsetY me
            , hue: if hue < 360.0
                     then hue + 1.0
                     else 0.0
            , direction: direction_
            , lineWidth: lineWidth_
            } _appState
      setLineWidth ctx lineWidth_
    else
      pure unit

initAppState :: Effect (Ref AppState)
initAppState =
   new { isDrawing: false
       , lastX: 0.0
       , lastY: 0.0
       , hue: 0.0
       , lineWidth: 100.0
       , direction: true
       }

modifyAppState
  :: Event
  →  Ref AppState
  →  Boolean
  →  Effect Unit
modifyAppState e _appState isDrawing_ = do
  let me = toMouseEvent e
  appState ← read _appState
  write (appState {isDrawing = isDrawing_, lastX = offsetX me, lastY = offsetY me}) _appState


main :: Effect Unit
main = do
  doc ← map toParentNode (window >>= document)
  _canvas ← querySelector (wrap "#draw") doc
  case (map fromElement _canvas) of
    Just canvasElement → do
      ctx ← getContext2D canvasElement
      setStrokeStyle ctx "#BADA55"
      setLineJoin ctx RoundJoin
      setLineCap ctx Round
      setLineWidth ctx 100.0
      _appState <- initAppState
      el1 ← eventListener \e → do
        let me = toMouseEvent e
        draw me ctx _appState (offsetX me) (offsetY me)
      el2 ← eventListener \e → modifyAppState e _appState false
      el3 ← eventListener \e → modifyAppState e _appState true
      case (map toEventTarget _canvas) of
        Just canvas → do
          addEventListener (wrap "mousedown") el3 false canvas
          addEventListener (wrap "mouseout") el2 false canvas
          addEventListener (wrap "mousemove") el1 false canvas
          addEventListener (wrap "mouseup") el2 false canvas
        Nothing → throw "no draw element"
    Nothing → throw "no draw element"
