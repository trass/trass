module Utils where

import Data.Time
import System.Locale

import Import
import Language

wAgo dt now = do
  [whamlet|
    <small .text-help title=#{formatTime defaultTimeLocale "%D %T %Z" dt}>
      #{ago dt now}
  |]
