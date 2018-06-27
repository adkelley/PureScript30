module Main where

import Prelude

import Data.Array (difference)
import Data.Date (year)
import Data.Enum (fromEnum)
import Data.Foldable (all, any, find)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (nowDate)

foreign import table :: ∀ a. Array a → Effect Unit

type BirthYear = Int

type Person =
  { name :: String
  , year :: BirthYear
  }


people :: Array Person
people =
  [ { name: "Wes",  year: 1988 }
  , { name: "Kait", year: 1986 }
  , { name: "Irv",  year: 1970 }
  , { name: "Lux",  year: 2015 }
  ]


type Comment =
  { text :: String
  , id   :: Int
  }

comments :: Array Comment
comments =
  [ { text: "Love this!", id: 523423 }
  , { text: "Super good", id: 823423 }
  , { text: "You are the best", id: 2039842 }
  , { text: "Ramen is my fav food ever", id: 123523 }
  , { text: "Nice Nice Nice!", id: 542328 }
  ]

nowYear :: Effect Int
nowYear = nowDate >>= year >>> fromEnum >>> pure

isAdult :: Array Person → Effect Boolean
isAdult xs = do
  ny <- nowYear
  pure $ any (\x → ny - x.year > 18) xs

allAdults :: Array Person → Effect Boolean
allAdults xs = do
  ny <- nowYear
  pure $ all (\x → ny - x.year > 18) xs

findComment :: Int → Array Comment → Maybe Comment
findComment id xs =
  find (\x → x.id == id) xs

newComments :: Int → Array Comment → Array Comment
newComments id xs = do
 let comment = findComment id xs
 case comment of
   Just ix → difference xs [ix]
   Nothing → xs

main :: Effect Unit
main = do
  log "07_Array_Cardio_Day_2"
  anyAdult ← isAdult people
  log $ "isAdult: " <> (show anyAdult)
  everyAdult ← allAdults people
  log $ "allAdults: " <> (show everyAdult)
  case (findComment 823423 comments) of
    Just x → log $ "comment: " <> (show x)
    Nothing → log "comment not found"
  table $ newComments 823423 comments
