module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Foldable (for_)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Number.Format (toString)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (new)
import Global (readFloat)
import Web.DOM.Element (Element, toEventTarget, toParentNode)
import Web.DOM.Node (toEventTarget) as WDN
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (querySelector, querySelectorAll)
import Web.Event.Event (Event, target)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode) as HD
import Web.HTML.Window (document)

type Elements =  { video :: EventTarget
                 , progress :: Element
                 , progressBar :: Element
                 , toggle :: Element
                 , skipButtons :: Array EventTarget
                 , ranges :: Array EventTarget
                 }


foreign import _isVideoPaused :: Fn1 EventTarget Boolean
foreign import playVideo :: EventTarget → Effect Unit
foreign import pauseVideo :: EventTarget → Effect Unit
foreign import videoCurrentTime :: EventTarget → Effect Number
foreign import setVideoCurrentTime :: EventTarget → Number → Effect Unit
foreign import videoDuration :: EventTarget → Effect Number
foreign import videoSkipTime :: EventTarget → Effect String
foreign import eventTargetName :: EventTarget → Effect String
foreign import eventTargetValue :: EventTarget → Effect Number
foreign import setVideoPlaybackRate :: EventTarget → Number → Effect Unit
foreign import setVideoVolume :: EventTarget → Number → Effect Unit
foreign import setFlexBasis :: Element → String → Effect Unit

foreign import setTextContent :: Element → String → Effect Unit

isVideoPaused :: EventTarget → Boolean
isVideoPaused = runFn1 _isVideoPaused

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
      ranges_ ← MaybeT $ querySelectorAll (wrap ".player__slider") player >>=
                              toArray >>= (WDN.toEventTarget <$> _) >>> Just >>> pure
      pure { video: video_, progress: progress_
           , progressBar: progressBar_, toggle: toggle_
           , skipButtons: skipButtons_, ranges: ranges_
           }
    Nothing → pure Nothing


togglePlay :: EventTarget → Effect Unit
togglePlay video =
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
  currentTime ← videoCurrentTime video
  duration ← videoDuration video
  let percent = (currentTime / duration) * 100.0
  setFlexBasis progressBar (toString percent)


skip :: EventTarget → Event → Effect Unit
skip video button = do
  currentTime <- videoCurrentTime video
  case (target button) of
    Just buttonEventTarget → do
      skipTime <- videoSkipTime buttonEventTarget
      setVideoCurrentTime video $ currentTime + (readFloat skipTime)
    Nothing → pure unit

-- | TODO: Use enumeration to determine volume or playback rate
handleRangeUpdate :: EventTarget → Event → Effect Unit
handleRangeUpdate video slider = do
  case (target slider) of
    Just sliderEventTarget → do
      name ← eventTargetName sliderEventTarget
      value ← eventTargetValue sliderEventTarget
      if name == "playbackRate"
        then setVideoPlaybackRate video value
        else setVideoVolume video value
    Nothing → pure unit



main :: Effect Unit
main = do
  elements ← getElements
  case elements of
    Just { video, progress, progressBar
         , toggle, skipButtons, ranges
         } → do
      togglePlayClick ← eventListener \_ → togglePlay video
      addEventListener (wrap "click") togglePlayClick false video
      playPause ← eventListener \_ → updateButton video toggle
      addEventListener (wrap "play") playPause false video
      addEventListener (wrap "pause") playPause false video
      hp ← eventListener \_ → handleProgress video progressBar
      addEventListener (wrap "timeupdate") hp false video

      addEventListener (wrap "click") togglePlayClick false (toEventTarget toggle)
      skipButton ← eventListener \buttonEvent → skip video buttonEvent
      for_ skipButtons $ addEventListener (wrap "click") skipButton false

      rangeChange ← eventListener \e → handleRangeUpdate video e
      for_ ranges $ addEventListener (wrap "change") rangeChange false
      rangeMouseMove ← eventListener \e → handleRangeUpdate video e
      for_ ranges $ addEventListener (wrap "mousemove") rangeMouseMove false

      -- mousedown_ ← new false
      -- scrubClick ← eventListener \e → video progress e
      -- addEventListener (wrap "click") scrubClick false progress
      -- scrubMouseMove ← \e → do
      --   mousedown ← read mouseDown_
      --   if mousedown
      --     then

    Nothing → throw "Error: check index.html for missing elements"
