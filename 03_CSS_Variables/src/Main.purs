module Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Internal.Types (NodeList)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (querySelectorAll)
import Web.Event.Event (target)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event, EventTarget)
import Web.HTML (HTMLInputElement, window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLInputElement (name, value)
import Web.HTML.Window (document)

foreign import datasetSizing :: HTMLInputElement → Effect String
foreign import setStyleProperty :: String → String → String → Effect Unit

handleUpdate :: Event → Effect Unit
handleUpdate e = do
  case (target e) of
    Just x → do
      let elt = unsafeCoerce x :: HTMLInputElement
      val ← value elt
      nam ← name elt
      suffix ← datasetSizing elt
      setStyleProperty nam val suffix
    Nothing → log "nothing"


nodesEventTargets :: NodeList → Effect (Array EventTarget)
nodesEventTargets = map unsafeCoerce $ toArray

main :: Effect Unit
main = do
  doc ← map toParentNode (window >>= document)
  inputs ← querySelectorAll (wrap(".controls input")) doc >>= nodesEventTargets
  el ← eventListener handleUpdate
  for_ inputs $ addEventListener (wrap "change") el false
  for_ inputs $ addEventListener (wrap "mousemove") el false
