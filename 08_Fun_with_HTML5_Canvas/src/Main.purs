module Main where

import Prelude

import Color (toHexString, hsv)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Class.Console (logShow)
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

toCanvasElement :: Element → CanvasElement
toCanvasElement = unsafeCoerce

toMouseEvent :: Event → MouseEvent
toMouseEvent = unsafeCoerce

draw :: MouseEvent → Context2D → Ref AppState → Number → Number → Effect Unit
draw me ctx _appState ox oy = do
  appState <- read _appState
  if appState.isDrawing
    then do
      setStrokeStyle ctx (toHexString $ hsv appState.hue 1.0 0.5)
      beginPath ctx
      moveTo ctx appState.lastX appState.lastY
      lineTo ctx ox oy
      stroke ctx
      let direction =
            if (appState.lineWidth >= 100.0 || appState.lineWidth <= 1.0)
              then not appState.direction
              else appState.direction
      let lineWidth =
            if direction
              then appState.lineWidth + 1.0
              else appState.lineWidth - 1.0
      write { isDrawing: true
            , lastX: offsetX me
            , lastY: offsetY me
            , hue: if appState.hue < 360.0
                     then appState.hue + 1.0
                     else 0.0
            , direction: direction
            , lineWidth: lineWidth
            } _appState
      setLineWidth ctx lineWidth
    else
      pure unit


main :: Effect Unit
main = do
  doc ← map toParentNode (window >>= document)
  _canvas ← querySelector (wrap "#draw") doc
  case (map toCanvasElement _canvas) of
    Just canvasElement → do
      ctx ← getContext2D canvasElement
      setStrokeStyle ctx "#BADA55"
      setLineJoin ctx RoundJoin
      setLineCap ctx Round
      setLineWidth ctx 100.0
      _appState <- new { isDrawing: false
                      , lastX: 0.0
                      , lastY: 0.0
                      , hue: 0.0
                      , lineWidth: 100.0
                      , direction: true
                      }
      el1 ← eventListener \e → do
                 let me = toMouseEvent e
                 draw me ctx _appState (offsetX me) (offsetY me)
      el2 ← eventListener \e → do
        appState <- read _appState
        write { isDrawing: false
              , lastX: offsetX $ toMouseEvent e
              , lastY: offsetY $ toMouseEvent e
              , hue: appState.hue
              , lineWidth: appState.lineWidth
              , direction: appState.direction
              } _appState
      el3 ← eventListener \e → do
        appState <- read _appState
        write { isDrawing: true
              , lastX: offsetX $ toMouseEvent e
              , lastY: offsetY $ toMouseEvent e
              , hue: appState.hue
              , direction: appState.direction
              , lineWidth: appState.lineWidth
              } _appState
      case (map toEventTarget _canvas) of
        Just canvas → do
          addEventListener (wrap "mousedown") el3 false canvas
          addEventListener (wrap "mouseout") el2 false canvas
          addEventListener (wrap "mousemove") el1 false canvas
          addEventListener (wrap "mouseup") el2 false canvas
        Nothing → throw "no draw element"
    Nothing → throw "no draw element"
