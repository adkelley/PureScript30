module DrumKit where

import Prelude hiding (div)

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Exception (throw)
import Text.Smolder.HTML (audio, div, kbd, source, span)
import Text.Smolder.HTML.Attributes (className, data', src, type')
import Text.Smolder.Markup (Markup, text, (!))
import Text.Smolder.Renderer.String (render)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (setClassName, className, fromParentNode, toEventTarget) as WDE
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (toEventTarget)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (ParentNode, querySelector, querySelectorAll)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key)

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

addPlaying :: Element -> Effect Unit
addPlaying key = do
  name <- WDE.className key
  WDE.setClassName (name <> " playing") key


removePlaying :: Element → Effect Unit
removePlaying = WDE.setClassName "key"

foreign import propertyName :: Event -> String
foreign import srcElement :: Event -> Element
foreign import innerHTML :: Element → String → Effect Unit

removeTransition :: Event -> Effect Unit
removeTransition e =
   if (propertyName e == "transform") then
     removePlaying $ srcElement e
   else
     pure unit

foreign import audioCurrentTime :: Int
                                -> Element
                                -> Effect Unit

foreign import playAudio :: Element -> Effect Unit


playSound :: (Maybe KeyboardEvent) -> ParentNode → Effect Unit
playSound keyboardEvent doc = do
  case keyboardEvent of
    Just ke → do
      let co = key ke
      audio <- querySelector (wrap ("audio[data=" <> "'" <> co <> "']")) doc
      case audio of
        Just el -> do
          audioCurrentTime 0 el
          key <- querySelector (wrap (".key[data=" <> "'" <> co <> "']")) doc
          case key of
            Just kel -> do
              addPlaying kel
            Nothing -> pure unit
          playAudio el
        Nothing -> pure unit
    Nothing → pure unit


main :: Effect Unit
main = do
  doc <- map toParentNode (window >>= document)
  container <- querySelector (wrap "#container") doc
  case container of
    Just el -> void $ do
      innerHTML el $ render view
      keys <- querySelectorAll (wrap ".key") doc >>= toArray
      el1 ← eventListener removeTransition
      for_  keys $ \x → addEventListener (wrap "transitionend") el1 false (toEventTarget x)
      el2 ← eventListener \e → playSound (fromEvent e) doc
      let docTarget = WDE.toEventTarget $ fromMaybe (unsafeCoerce doc :: Element) $ WDE.fromParentNode doc
      addEventListener (wrap "keydown") el2 false docTarget
    Nothing -> throw "No 'container' node!"
