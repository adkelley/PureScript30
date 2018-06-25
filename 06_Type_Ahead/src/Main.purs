module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Regex (match, replace)
import Data.String.Regex.Flags (RegexFlags(..), global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (F)
import Milkis (Fetch, URL(..))
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Simple.JSON (read')
import Text.Smolder.HTML (li, span)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup, text, (!))
import Text.Smolder.Renderer.String (render)
import Web.DOM.Element (Element, toEventTarget)
import Web.DOM.ParentNode (querySelector)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.HTML.Window (document)

type City = { city :: String
            , growth_from_2000_to_2013 :: String
            , latitude :: Number
            , longitude :: Number
            , population :: String
            , rank :: String
            , state :: String
            }

type Cities = Array City

type Word = String

foreign import innerHTML :: Element → String → Effect Unit

endpoint :: URL
endpoint = URL "https://gist.githubusercontent.com/Miserlou/c5cd8364bf9b2420bb29/raw/2bf258763cdddd704f8ffd3ea9a3e81d25e2c6f6/cities.json"

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


findMatches :: Word → Cities → Cities
findMatches wordToMatch cities =
  filter (matchFn regexp) cities where
    regexp = unsafeRegex wordToMatch giFlags
    matchFn word = \x → (match word x.city) /= Nothing   ||
                        (match word x.state) /= Nothing


numberWithCommas :: String → String
numberWithCommas x =
  replace regexp "," x where
    regexp = unsafeRegex "\\B(?=(\\d{3})+(?!\\d))" global


cityStatePop :: ∀ e. Markup e → Markup e → String → Markup e
cityStatePop city state population =
  li $ do
    span ! className "name" $ do
      city
      text ", "
      state
    span ! className "population" $ text $ numberWithCommas population


highlight :: ∀ e. String → String → Markup e
highlight place chars = do
  let regexp = unsafeRegex chars giFlags
  let xs = toCharArray $ replace regexp "@" place
  for_ xs \x → if (x /= '@')
                 then text $ singleton x
                 else span ! className "hl" $ text chars


markup :: ∀ e. City → String → Markup e
markup place chars = do
  let cityName = highlight place.city chars
  let stateName = highlight place.state chars
  cityStatePop cityName stateName place.population


displayMatches :: EventTarget → Element → Cities → Effect Unit
displayMatches searchInput suggestions cities = do
  case (fromEventTarget searchInput) of
    Just this → do
      chars ← (value this)
      let matches = findMatches chars cities
      let html = for_ matches (\place → markup place chars)
      innerHTML suggestions $ render html
    Nothing → pure unit


fetchCities :: Aff (Either String Cities)
fetchCities = do
  _response ← attempt $ fetch endpoint M.defaultFetchOptions
  case _response of
    Left e →
      pure $ Left $ show e
    Right response → do
      stuff ← M.json response
      case (runExcept $ (read' stuff) :: F Cities) of
        Left e →
          pure $ Left $ show e
        Right cities →
          pure $ Right cities

mainAff :: Aff Unit
mainAff = do
  response ← fetchCities
  case response of
    Left e → liftEffect $ throw $ "failed: " <> e
    Right cities → liftEffect $ do
      doc ← map toParentNode (window >>= document)
      maybeSelectors ← runMaybeT $ do
        s1 ← MaybeT $ querySelector (wrap ".search") doc
        s2 ← MaybeT $ querySelector (wrap ".suggestions") doc
        pure { searchInput: toEventTarget s1, suggestions: s2  }
      case maybeSelectors of
        Just { searchInput, suggestions } → do
           el ← eventListener \_ → displayMatches searchInput suggestions cities
           addEventListener (wrap "change") el false searchInput
           addEventListener (wrap "keyup") el false searchInput
        Nothing → throw "No .search or .suggestions selectors"


main :: Effect Unit
main =
  void $ launchAff mainAff
