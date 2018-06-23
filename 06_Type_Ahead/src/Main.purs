module Main where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (RegexFlags(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Foreign (Foreign)
import Milkis (Fetch, URL(..))
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (querySelector)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

type City = { city :: String
            , state :: String
            }
type Cities = Array City

type Word = String

foreign import innerHTML :: String → EventTarget → Effect Unit

endpoint :: URL
endpoint = URL "https://gist.githubusercontent.com/Miserlou/c5cd8364bf9b2420bb29/raw/2bf258763cdddd704f8ffd3ea9a3e81d25e2c6f6/cities.json"

dummyCities :: Cities
dummyCities = [{ city: "Boston", state: "Mass"}, { city: "San Francisco", state: "California"}]

fetch :: Fetch
fetch = M.fetch nodeFetch

giFlags :: RegexFlags
giFlags = RegexFlags
  { global: true
  , ignoreCase: true
  , multiline: false
  , sticky: false
  , unicode: false
  }

findMatches :: Word → Cities → Effect Unit
findMatches wordToMatch cities =
  case regexp of
    Left e → throw $ "failed regexp" <> show e
    Right r → log "in find matches"
  where
    regexp = regex wordToMatch giFlags


displayMatches :: Event → Word → Cities → EventTarget → Effect Unit
displayMatches wordToMatch cities suggestions = do
  let html = "<li><span class=\"name\"><span class=\"h1\">San Francisco, California</span></span></li>"
  innerHTML html suggestions


makeCities :: Foreign → Cities
makeCities blob = dummyCities


fetchCities :: Aff (Either String Cities)
fetchCities = do
  _response ← attempt $ fetch endpoint M.defaultFetchOptions
  case _response of
    Left e → do
      pure $ Left $ "failed with " <> show e
    Right response → do
      stuff ← M.json response
      pure $ Right $ makeCities stuff

mainAff :: Aff Unit
mainAff = do
  response ← fetchCities
  case response of
    Left e → liftEffect $ throw $ "failed with " <> show e
    Right cities → liftEffect $ do
      doc ← map toParentNode (window >>= document)
      maybeSelectors ← runMaybeT $ do
        s1 ← MaybeT $ querySelector (wrap ".search") doc
        s2 ← MaybeT $ querySelector (wrap ".suggestions") doc
        pure { searchInput: toEventTarget s1, suggestions: toEventTarget s2  }
      case maybeSelectors of
        Just { searchInput, suggestions } → do
           el ← eventListener \e → displayMatches e "California" cities suggestions
           addEventListener (wrap "change") el false searchInput
           addEventListener (wrap "keyup") el false searchInput
        Nothing → throw "No .search or .suggestions selectors"


main :: Effect Unit
main =
  void $ launchAff mainAff
