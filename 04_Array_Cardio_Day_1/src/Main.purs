module Main where

import Prelude

import Data.Array (filter, foldr, head, sortBy)
import Data.Maybe (fromMaybe)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log, logShow)

type Inventor = { first :: String
                , last :: String
                , year :: Int
                , passed :: Int
                }

type Inventors = Array Inventor

type FullName = { first :: String
                , last  :: String
                }

type FullNames = Array FullName

type Transport = { car :: Int
                 , truck :: Int
                 , bike :: Int
                 , walk :: Int
                 , van :: Int
                 , pogostick :: Int
                 }

foreign import table :: ∀ a. Array a  → Effect Unit

inventors :: Inventors
inventors =  [
      { first: "Albert", last: "Einstein", year: 1879, passed: 1955
    },
      { first: "Isaac", last: "Newton", year: 1643, passed: 1727 },
      { first: "Galileo", last: "Galilei", year: 1564, passed: 1642 },
      { first: "Marie", last: "Curie", year: 1867, passed: 1934 },
      { first: "Johannes", last: "Kepler", year: 1571, passed: 1630 },
      { first: "Nicolaus", last: "Copernicus", year: 1473, passed: 1543 },
      { first: "Max", last: "Planck", year: 1858, passed: 1947 },
      { first: "Katherine", last: "Blodgett", year: 1898, passed: 1979 },
      { first: "Ada", last: "Lovelace", year: 1815, passed: 1852 },
      { first: "Sarah E.", last: "Goode", year: 1855, passed: 1905 },
      { first: "Lise", last: "Meitner", year: 1878, passed: 1968 },
      { first: "Hanna", last: "Hammarström", year: 1829, passed: 1909 }
    ]

people :: Array String
people = [ "Beck, Glenn", "Becker, Carl", "Beckett, Samuel", "Beddoes, Mick", "Beecher, Henry"
         , "Beethoven, Ludwig", "Begin, Menachem", "Belloc, Hilaire", "Bellow, Saul", "Benchley"
         , "Robert", "Benenson, Peter", "Ben-Gurion, David", "Benjamin, Walter", "Benn, Tony"
         , "Bennington, Chester", "Benson, Leana", "Bent, Silas", "Bentsen, Lloyd", "Berger, Ric"
         , "Bergman, Ingmar", "Berio, Luciano", "Berle, Milton", "Berlin, Irving", "Berne, Eric"
         , "Bernhard, Sandra", "Berra, Yogi", "Berry, Halle", "Berry, Wendell", "Bethea, Erin"
         , "Bevan, Aneurin", "Bevel, Ken", "Biden, Joseph", "Bierce, Ambrose", "Biko, Steve"
         , "Billings, Josh", "Biondo, Frank", "Birrell, Augustine", "Black Elk", "Blair, Robert"
         , "Blair, Tony", "Blake, William"
         ]

data_ :: Array String
data_ =  [ "car", "car", "truck", "truck", "bike", "walk", "car", "van", "bike", "walk", "car"
         , "van", "car", "truck", "pogostick"
         ]

-- | 1. Filter the list of inventors for those who are born in the 1500s
fifteen :: Inventors → Inventors
fifteen =  filter (\x → x.year > 1499 && x.year < 1600)

-- | 2. Give us an array of the inventors first and last names
fullNames :: Inventors → FullNames
fullNames = map (\x → {first: x.first, last: x.last})

-- | 3. Sort the inventors by birthdate, oldest to youngest
ordered :: Inventors → Inventors
ordered = sortBy birth where
  birth a b = compare b.year a.year

-- | 4. How many years did all the inventors live?
totalYears :: Inventors → Int
totalYears = foldr (\x → (+) (x.passed - x.year)) 0

-- | 5. Sort inventors by years lived
oldest :: Inventors → Inventors
oldest = sortBy age where
  age a b = compare (b.passed - b.year) (a.passed - a.year)

-- | 6

-- | 7. Sort the people alphabetically by last name
alpha :: Array String → Array String
alpha = sortBy lastName where
  lastName a b =
    let aLastName = head $ split (Pattern ", ") a
        bLastName = head $ split (Pattern ", ") b
    in
      compare (fromMaybe "" aLastName) (fromMaybe "" bLastName)

-- | 8. Sum up the instances of each of these
-- | Todo: How to tranverse an array while updating record
showTransport :: Transport → String
showTransport x = "{ car: " <> (show x.car) <> " " <>
                  "truck: " <> (show x.truck) <> " " <>
                  "bike: " <> (show x.bike) <> " " <>
                  "walk: " <> (show x.walk) <> " " <>
                  "van: " <> (show x.van) <> " " <>
                  "pogostick: " <> (show x.pogostick) <> " " <>
                  " }"

initTransport :: Transport
initTransport = { car: 0, truck: 0, bike: 0, walk: 0
                , van: 0, pogostick: 0
                }

transportation :: Array String → Transport
transportation = \_ → initTransport


main :: Effect Unit
main = do
  log "Array Cardio Workout Day 1"
  log "\n1. Inventors born in 1500's"
  table $ fifteen inventors
  log "\n2. Inventors full names"
  table $ fullNames inventors
  log "\n3. Inventors sorted by birth date"
  table $ ordered inventors
  log "\n4. Total years lived"
  logShow $ totalYears inventors
  log "\n5. Sorted by years lived"
  table $ oldest inventors
  log "\n6. Not applicable"
  log "\n7. People sorted by last name"
  logShow $ alpha people
  log "\n8. Sum up instances of each of these"
  log $ showTransport $ transportation data_
