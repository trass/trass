module Utils where

import Import
import Data.Time

ago :: UTCTime -> UTCTime -> AppMessage
ago t now
  | minutes == 0  = MsgTimeAgoJustNow
  | minutes <  5  = MsgTimeAgoMinutes minutes
  | hours == 0    = MsgTimeAgoMinutes (5 * (minutes `div` 5))
  | days == 0     = MsgTimeAgoHours hours
  | weeks == 0    = MsgTimeAgoDays days
  | months == 0   = MsgTimeAgoWeeks weeks
  | years == 0    = MsgTimeAgoMonths months
  | otherwise     = MsgTimeAgoYears years
  where
    seconds = floor (diffUTCTime now t)
    minutes = seconds `div` 60
    hours   = minutes `div` 60
    days    = hours `div` 24
    weeks   = days `div` 7
    months  = days `div` 30
    years   = days `div` 365

wAgo :: (Text -> AppMessage) -> UTCTime -> UTCTime -> WidgetT App IO ()
wAgo f dt now = do
  mr <- getMessageRender
  let agoText = mr $ ago dt now
  [whamlet|
    <small .text-help title=#{formatTimeFull dt}>
      _{f agoText}
  |]

