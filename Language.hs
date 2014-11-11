module Language where

import Prelude
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Time

langTitles :: Map Text Text
langTitles = Map.fromList
  [ ("en", "English")
  , ("es", "Español")
  , ("ru", "Русский")
  ]

data TimeAgo
  = TAJustNow
  | TAMinutes Int
  | TAHours Int
  | TADays Int
  | TAWeeks Int
  | TAMonths Int
  | TAYears Int
  deriving (Show)

pluralEn :: Int -> Text -> Text -> Text
pluralEn 1 s _ = s
pluralEn n _ s = Text.pack (show n) <> " " <> s

pluralRu :: Int -> Text -> Text -> Text -> Text
pluralRu 1 s1 _  _ = s1
pluralRu n s1 s2 s
  | one       = prefix <> s1
  | few       = prefix <> s2
  | otherwise = prefix <> s
  where
    prefix = Text.pack (show n) <> " "
    tenth = m > 10 && m < 20
    one   = not tenth && k == 1
    few   = not tenth && 1 < k && k < 5
    m = n `mod` 100
    k = n `mod` 10

ago :: UTCTime -> UTCTime -> TimeAgo
ago t now
  | minutes == 0  = TAJustNow
  | minutes <  5  = TAMinutes minutes
  | hours == 0    = TAMinutes (5 * (minutes `div` 5))
  | days == 0     = TAHours hours
  | weeks == 0    = TADays days
  | months == 0   = TAWeeks weeks
  | years == 0    = TAMonths months
  | otherwise     = TAYears years
  where
    seconds = floor (diffUTCTime now t)
    minutes = seconds `div` 60
    hours   = minutes `div` 60
    days    = hours `div` 24
    weeks   = days `div` 7
    months  = days `div` 30
    years   = days `div` 365
