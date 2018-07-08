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
  let element_ = currentTarget e >>= \x → fromEventTarget x
  case (element_) of
    Just element → do
      val ← value element
      nam ← name element
      suffix ← datasetSizing element
      setStyleProperty nam val suffix
    Nothing → pure unit


main :: Effect Unit
main = do
  doc ← map toParentNode (window >>= document)
  let inputs = querySelectorAll (wrap ".controls input") doc >>=
           toArray >>= (toEventTarget <$> _)
  -- inputs ← querySelectorAll (wrap ".controls input") doc >>=
  --          toArray >>= (toEventTarget <$> _) >>> pure
  el ← eventListener handleUpdate
  for_ inputs $ addEventListener (wrap "change") el false
  for_ inputs $ addEventListener (wrap "mousemove") el false
