module DrumKit where

import Prelude hiding (div)

import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)
import Control.Monad.Except (runExcept)
import Web.Event.Internal.Types (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Types (EventTarget)
import Web.HTML (window)
import Web.UIEvent.KeyboardEvent as KE
import Web.DOM.Document (document, toNonElementParentNode, toParentNode, windowToEventTarget)
import Web.DOM.Element (setClassName, className) as DNE
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (querySelector, querySelectorAll)
import Web.DOM.Node (Element, NodeList, ParentNode)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Text.Smolder.HTML (audio, div, kbd, source, span)
import Text.Smolder.HTML.Attributes (className, data', src, type')
import Text.Smolder.Markup (Markup, text, (!))
import Text.Smolder.Renderer.DOM (render)
import Unsafe.Coerce (unsafeCoerce)

type AudioTag = { dataKey :: String, pathToFile :: String }

setAudioTag :: ∀ e. String -> String -> Markup e
setAudioTag dataKey pathToFile =
  audio ! data' dataKey $
    source ! src pathToFile ! type' "audio/wav"

setAudioTags :: ∀ e. Markup e
setAudioTags = do
  setAudioTag "a" "sounds/clap.wav"
  setAudioTag "s" "sounds/hihat.wav"
  setAudioTag "d" "sounds/kick.wav"
  setAudioTag "f" "sounds/openhat.wav"
  setAudioTag "g" "sounds/boom.wav"
  setAudioTag "h" "sounds/ride.wav"
  setAudioTag "j" "sounds/snare.wav"
  setAudioTag "k" "sounds/tom.wav"
  setAudioTag "l" "sounds/tink.wav"

setDataKey :: ∀ e. String -> String -> String -> Markup e
setDataKey ascii key sound =
  div ! className "key" ! data' ascii $ do
    kbd $ text key
    span ! className "sound" $ text sound

setDataKeys :: ∀ e. Markup e
setDataKeys = do
  div ! className "keys" $ do
    setDataKey "a" "A" "clap"
    setDataKey "s" "S" "hihat"
    setDataKey "d" "D" "kick"
    setDataKey "f" "F" "openhat"
    setDataKey "g" "G" "boom"
    setDataKey "h" "H" "ride"
    setDataKey "j" "J" "snare"
    setDataKey "k" "K" "tom"
    setDataKey "l" "L" "tink"

view :: ∀ e. Markup e
view = do
  setDataKeys
  setAudioTags

addPlaying :: forall e
           .  Element
           -> Effect Unit
addPlaying key = do
  name <- DNE.className key
  DNE.setClassName (name <> " playing") key


removePlaying :: forall e
           .  Element
           -> Effect Unit
removePlaying = DNE.setClassName "key"

foreign import propertyName :: Event -> String
foreign import srcElement :: Event -> Element

removeTransition :: forall e. Event -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
removeTransition e =
   if (propertyName e == "transform") then
     removePlaying $ srcElement e
   else
     pure unit

foreign import audioCurrentTime :: Int
                                -> Element
                                -> Effect Unit

foreign import playAudio :: Element
                         -> Effect Unit


playSound :: Event -> Effect Unit
playSound e = do
  for_ (runExcept (KE.eventToKeyboardEvent e)) \ke -> do
    let co = KE.key ke
    document <- map htmlDocumentToParentNode (window >>= document)
    audio <- querySelector (wrap ("audio[data=" <> "'" <> co <> "']")) document
    case audio of
      Just el -> do
        audioCurrentTime 0 el
        key <- querySelector (wrap (".key[data=" <> "'" <> co <> "']")) document
        case key of
          Just kel -> do
            addPlaying kel
          Nothing -> void do
            error "No key element"
        playAudio el
      Nothing -> pure unit

nodesToEventTarget :: NodeList -> Effect (Array EventTarget)
nodesToEventTarget = map unsafeCoerce $ toArray

main :: Effect Unit
main = do
  document <- map htmlDocumentToNonElementParentNode (window >>= document)
  container <- getElementById (wrap "container") document
  case container of
    Just el -> void do
      render el view
      keys <- querySelectorAll (wrap ".key") (unsafeCoerce document :: ParentNode) >>= nodesToEventTarget
      for_  keys $ addEventListener (wrap "transitionend") (eventListener removeTransition) false
      map windowToEventTarget window >>= addEventListener (wrap "keydown") (eventListener playSound) false
    Nothing -> throw "No 'container' node!"
