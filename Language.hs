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

