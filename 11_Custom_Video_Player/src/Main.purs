module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Web.DOM.Element (Element, toEventTarget, toParentNode)
import Web.DOM.ParentNode (querySelector)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode) as HD
import Web.HTML.Window (document)

type Elements =  { video :: EventTarget
                 , progress :: Element
                 , progressBar :: Element
                 , toggle :: Element
                 , skipButtons :: Element
                 , ranges :: Element
                 }


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
      skipButtons_ ← MaybeT $ querySelector (wrap "[data-skip]") player
      ranges_ ← MaybeT $ querySelector (wrap ".player__slider") player
      pure { video: video_, progress: progress_
           , progressBar: progressBar_, toggle: toggle_
           , skipButtons: skipButtons_, ranges: ranges_
           }
    Nothing → pure Nothing


togglePlay :: Event → Effect Unit
togglePlay e = do
  log "togglePlay"

updateButton :: Event → Effect Unit
updateButton e = do
  log "updateButton"

handleProgress :: Event → Effect Unit
handleProgress e = do
  log "handleProgress"

main :: Effect Unit
main = do
  elements ← getElements
  case elements of
    Just { video, progress, progressBar
         , toggle, skipButtons, ranges
         } → do
      tpel ← eventListener \playButton → togglePlay playButton
      addEventListener (wrap "click") tpel false video
      ubel ← eventListener \playButton → updateButton playButton
      addEventListener (wrap "play") ubel false video
      addEventListener (wrap "pause") ubel false video
      upel ← eventListener \e → handleProgress e
      addEventListener (wrap "timeupdate") upel false video
    Nothing → throw "Check index.html for missing elements"
