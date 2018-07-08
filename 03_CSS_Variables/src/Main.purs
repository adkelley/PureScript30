module Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Web.DOM.Node (toEventTarget)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (querySelectorAll)
import Web.Event.Event (currentTarget)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (HTMLInputElement, window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLInputElement (fromEventTarget, name, value)
import Web.HTML.Window (document)

foreign import datasetSizing :: HTMLInputElement → Effect String
foreign import setStyleProperty :: String → String → String → Effect Unit


handleUpdate :: Event → Effect Unit
handleUpdate e = do
  case (currentTarget e >>= fromEventTarget) of
    Just inputElement → do
      val ← value inputElement
      nam ← name inputElement
      suffix ← datasetSizing inputElement
      setStyleProperty nam val suffix
    Nothing → pure unit


main :: Effect Unit
main = do
  doc ← map toParentNode (window >>= document)
  inputs ← querySelectorAll (wrap ".controls input") doc >>=
           toArray >>= (toEventTarget <$> _) >>> pure
  el ← eventListener handleUpdate
  for_ inputs $ addEventListener (wrap "change") el false
  for_ inputs $ addEventListener (wrap "mousemove") el false
