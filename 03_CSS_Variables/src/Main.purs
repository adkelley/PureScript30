module Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Console (error)
import Web.DOM.Internal.Types (NodeList)
import Web.DOM.Node (toEventTarget)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (querySelectorAll)
import Web.Event.Event (currentTarget)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event, EventTarget)
import Web.HTML (HTMLInputElement, window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLInputElement (fromEventTarget, name, value)
import Web.HTML.Window (document)

foreign import datasetSizing :: HTMLInputElement → Effect String
foreign import setStyleProperty :: String → String → String → Effect Unit

handleUpdate :: Event → Effect Unit
handleUpdate e = do
  case (currentTarget e) of
    Just tgt → do
      case (fromEventTarget tgt) of
        Just elt → do
          val ← value elt
          nam ← name elt
          suffix ← datasetSizing elt
          setStyleProperty nam val suffix
        Nothing → error "no HTMLInputelement"
    Nothing → error "this shouldn't happen"

toEventTargets :: NodeList → Effect (Array EventTarget)
toEventTargets nodeList = do
  nodes ← toArray nodeList
  pure $ map toEventTarget nodes

main :: Effect Unit
main = do
  doc ← map toParentNode (window >>= document)
  inputs ← querySelectorAll (wrap ".controls input") doc >>= toEventTargets
  el ← eventListener handleUpdate
  for_ inputs $ addEventListener (wrap "change") el false
  for_ inputs $ addEventListener (wrap "mousemove") el false
