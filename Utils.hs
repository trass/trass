module Utils where

import Data.Maybe
import Data.Time
import System.Locale

import Import
import Language

agoMessage :: TimeAgo -> AppMessage
agoMessage TAJustNow = MsgTimeAgoJustNow
agoMessage (TAMinutes n) = MsgTimeAgoMinutes n
agoMessage (TAHours   n) = MsgTimeAgoHours n
agoMessage (TADays    n) = MsgTimeAgoDays n
agoMessage (TAWeeks   n) = MsgTimeAgoWeeks n
agoMessage (TAMonths  n) = MsgTimeAgoMonths n
agoMessage (TAYears   n) = MsgTimeAgoYears n

-- wAgo :: UTCTime -> UTCTime -> ???
wAgo f dt now = do
  mr <- getMessageRender
  let agoText = mr $ agoMessage $ ago dt now
  [whamlet|
    <small .text-help title=#{formatTime defaultTimeLocale "%D %T %Z" dt}>
      _{f agoText}
  |]
