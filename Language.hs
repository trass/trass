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

ago :: UTCTime -> UTCTime -> Text
ago t now
  | minutes == 0  = "just now"
  | minutes <  2  = "a minute ago"
  | minutes <  5  = Text.pack (show minutes) <> " minutes ago"
  | hours == 0    = Text.pack (show $ 5 * (minutes `div` 5)) <> " minutes ago"
  | hours == 1    = "an hour ago"
  | days == 0     = Text.pack (show hours) <> " hours ago"
  | days == 1     = "a day ago"
  | weeks == 0    = Text.pack (show days) <> " days ago"
  | weeks == 1    = "a week ago"
  | months == 0   = Text.pack (show weeks) <> " weeks ago"
  | months == 1   = "a month ago"
  | years == 0    = Text.pack (show months) <> " months ago"
  | years == 1    = "a year ago"
  | otherwise     = Text.pack (show years) <> " years ago"
  where
    seconds = floor (diffUTCTime now t)
    minutes = seconds `div` 60
    hours   = minutes `div` 60
    days    = hours `div` 24
    weeks   = days `div` 7
    months  = days `div` 30
    years   = days `div` 365
