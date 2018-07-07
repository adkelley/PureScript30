module Clock where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.JSDate (getHours, getMinutes, getSeconds, now)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Number.Format (toString)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Timer (setInterval)
import Web.DOM.Element (Element, setAttribute)
import Web.DOM.ParentNode (querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

type ClockElements = {sh :: Element, mh :: Element, hh :: Element}

setDate :: ClockElements -> Effect Unit
setDate {sh, mh, hh} = do
 time <- now
 seconds <- getSeconds time
 minutes <- getMinutes time
 hours <- getHours time
 let secondsDegrees = ((seconds / 60.0) * 360.0) + 90.0
 let minutesDegrees = ((minutes / 60.0) * 360.0) + 90.0
 let hoursDegrees = ((hours / 12.0) * 360.0) + 90.0
 setAttribute "style" ("transform:rotate(" <> (toString secondsDegrees) <> "deg)") sh
 setAttribute "style" ("transform:rotate(" <> (toString minutesDegrees) <> "deg)") mh
 setAttribute "style" ("transform:rotate(" <> (toString hoursDegrees) <> "deg)") hh


main :: Effect Unit
main = do
  doc <- map toParentNode (window >>= document)
  maybeSelectors <- runMaybeT $ do
    secondHand <- MaybeT $ querySelector (wrap ".second-hand") doc
    minuteHand <- MaybeT $ querySelector (wrap ".minute-hand") doc
    hourHand   <- MaybeT $ querySelector (wrap ".hour-hand") doc
    pure { sh : secondHand, mh : minuteHand, hh : hourHand }
  case maybeSelectors of
    Just {sh, mh, hh } -> do
     id <- setInterval 1000 (setDate {sh, mh, hh} )
     setDate {sh, mh, hh}
    Nothing -> throw "No second-hand, minute-hand or hour-hand nodes"
