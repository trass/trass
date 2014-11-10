module Language where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

langTitles :: Map Text Text
langTitles = Map.fromList
  [ ("en", "English")
  , ("es", "Español")
  , ("ru", "Русский")
  ]

