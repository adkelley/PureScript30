module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_, write)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Node (toEventTarget)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (querySelectorAll)
import Web.Event.Event (Event, currentTarget)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (HTMLInputElement, window)
import Web.HTML.HTMLDocument (toEventTarget, toParentNode) as HD
import Web.HTML.HTMLInputElement (checked, fromEventTarget, setChecked)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent, fromEvent, shiftKey)

type CheckboxStuff = { me :: MouseEvent, ct :: EventTarget, ie :: HTMLInputElement }

foreign import eqEventTarget :: EventTarget → EventTarget → Boolean

handleCheck :: Event → Array EventTarget → Ref EventTarget → Effect Unit
handleCheck e checkboxes _lastChecked = do
  maybeCheckboxStuff ← runMaybeT $ do
    mouseEvent ←  MaybeT $ pure $ fromEvent e
    target ← MaybeT $ pure $ currentTarget e
    inputElement ← MaybeT $ pure $ fromEventTarget target
    pure { me: mouseEvent, ct: target, ie: inputElement }
  case maybeCheckboxStuff of
    Just {me, ct, ie} → do
      isChecked ← checked ie
      inBetween ←  new false
      if (shiftKey me && isChecked)
        then do
          lastChecked ← read _lastChecked
          for_ checkboxes \checkbox → do
            if (eqEventTarget checkbox ct || eqEventTarget checkbox lastChecked)
              then modify_ not inBetween
              else pure unit
            read inBetween >>= \x →
              if x
                then setChecked true (unsafeCoerce checkbox :: HTMLInputElement)
                else pure unit
        else pure unit
      write ct _lastChecked
    Nothing → pure unit


main :: Effect Unit
main = do
  doc ← (window >>= document)
  checkboxes ← querySelectorAll (wrap ".inbox input[type=\"checkbox\"]") (HD.toParentNode doc) >>=
               toArray >>= (toEventTarget <$> _) >>> pure

  lastChecked ← new $ HD.toEventTarget doc
  el ← eventListener \e → handleCheck e checkboxes lastChecked
  for_  checkboxes $ addEventListener (wrap "click") el false
