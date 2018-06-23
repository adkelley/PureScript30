module Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Web.DOM (NodeList)
import Web.DOM.DOMTokenList (toggle)
import Web.DOM.Node (toEventTarget)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (querySelectorAll)
import Web.Event.Event (Event, currentTarget)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (classList, fromEventTarget)
import Web.HTML.Window (document)

foreign import logIt :: ∀ a. a → Effect Unit
foreign import propertyName :: Event → Effect String

toggleElement :: String → Event → Effect Boolean
toggleElement str e = do
  case (currentTarget e >>= fromEventTarget) of
    Just x → do
      tokenList ← classList x
      toggle tokenList str
    Nothing → pure false

toggleOpen :: Event → Effect Boolean
toggleOpen =
  toggleElement "open"

toggleActive :: Event → Effect Boolean
toggleActive e = do
  pname ← propertyName e
  if (contains (Pattern "flex") pname)
    then toggleElement "open-active" e
    else pure false

toEventTargets :: NodeList → Effect (Array EventTarget)
toEventTargets nodeList = do
  nodes ← toArray nodeList
  pure $ map toEventTarget nodes

main :: Effect Unit
main = do
  doc ← map toParentNode (window >>= document)
  panels ← querySelectorAll (wrap ".panel") doc >>= toEventTargets
  el1 ←  eventListener toggleOpen
  for_ panels $ addEventListener (wrap "click") el1 false
  el2 ← eventListener toggleActive
  for_ panels $ addEventListener (wrap "transitionend") el2 false
