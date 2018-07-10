module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (throw)
import Web.DOM.Element (Element, toEventTarget, toParentNode)
import Web.DOM.Node (toEventTarget) as WDN
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (querySelector, querySelectorAll)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode) as HD
import Web.HTML.Window (document)

type Elements =  { video :: EventTarget
                 , progress :: Element
                 , progressBar :: Element
                 , toggle :: Element
                 , skipButtons :: Array EventTarget
                 , ranges :: Element
                 }


foreign import isVideoPaused :: EventTarget → Boolean
foreign import playVideo :: EventTarget → Effect Unit
foreign import pauseVideo :: EventTarget → Effect Unit
foreign import setTextContent :: Element → String → Effect Unit

getElements :: Effect (Maybe Elements)
getElements = do
  doc ← map HD.toParentNode (window >>= document)
  player_ ← (querySelector (wrap ".player") doc) >>=
            (toParentNode <$> _)  >>> pure
  case player_ of
    Just player → runMaybeT $ do
      video_ ← MaybeT $ querySelector (wrap ".viewer") player >>= (toEventTarget <$> _) >>> pure
      progress_ ← MaybeT $ querySelector (wrap ".progress")  player
      progressBar_ ← MaybeT $ querySelector (wrap ".progress__filled") player
      toggle_ ← MaybeT $ querySelector (wrap ".toggle") player
      skipButtons_ ← MaybeT $ querySelectorAll (wrap "[data-skip]") player >>=
                              toArray >>= (WDN.toEventTarget <$> _) >>> Just >>> pure
      ranges_ ← MaybeT $ querySelector (wrap ".player__slider") player
      pure { video: video_, progress: progress_
           , progressBar: progressBar_, toggle: toggle_
           , skipButtons: skipButtons_, ranges: ranges_
           }
    Nothing → pure Nothing


togglePlay :: EventTarget → Effect Unit
togglePlay video = do
  logShow $ isVideoPaused video
  if (isVideoPaused video)
    then playVideo video
    else pauseVideo video


updateButton :: EventTarget → Element → Effect Unit
updateButton video toggle =
  if (isVideoPaused video)
    then setTextContent toggle "►"
    else setTextContent toggle "❚ ❚"


handleProgress :: EventTarget → Element → Effect Unit
handleProgress video progressBar = do
  log "handleProgress"

skip :: EventTarget → Effect Unit
skip video =
  log "in skip"

main :: Effect Unit
main = do
  elements ← getElements
  case elements of
    Just { video, progress, progressBar
         , toggle, skipButtons, ranges
         } → do
      tp ← eventListener \_ → togglePlay video
      addEventListener (wrap "click") tp false video
      ub ← eventListener \_ → updateButton video toggle
      addEventListener (wrap "play") ub false video
      addEventListener (wrap "pause") ub false video
      hp ← eventListener \_ → handleProgress video progressBar
      addEventListener (wrap "timeupdate") hp false video

      addEventListener (wrap "click") tp false (toEventTarget toggle)
      s ← eventListener \_ → skip video
      for_ skipButtons $ addEventListener (wrap "click") s false
    Nothing → throw "Error: check index.html for missing elements"
