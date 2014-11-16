module Utils where

import Import
import Control.Monad
import Control.Arrow ((&&&))
import Data.Time
import Data.Int
import qualified Data.Text as Text
import qualified Data.Map as Map
import SubmissionStatus
import UserRole
import Achievement

import Text.Blaze.Html.Renderer.Text (renderHtml)

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

wAgo :: (Text -> AppMessage) -> UTCTime -> UTCTime -> Widget
wAgo f dt now = do
  mr <- getMessageRender
  let agoText = mr $ ago dt now
  [whamlet|
    <small .text-help title=#{formatTimeFull dt}>
      _{f agoText}
  |]

wDuration :: Int64 -> Widget
wDuration n = do
  mr <- getMessageRender
  let mkDuration w d h m s = Text.intercalate " " $ filter (not . Text.null)
        [ if weeks    > 0 then mr (w weeks)   else ""
        , if days     > 0 then mr (d days)    else ""
        , if hours    > 0 then mr (h hours)   else ""
        , if minutes  > 0 then mr (m minutes) else ""
        , if seconds  > 0 then mr (s seconds) else ""
        ]
      shortDuration = mkDuration MsgDurationWeeks MsgDurationDays MsgDurationHours MsgDurationMinutes MsgDurationSeconds
      fullDuration  = mkDuration MsgFullDurationWeeks MsgFullDurationDays MsgFullDurationHours MsgFullDurationMinutes MsgFullDurationSeconds

  [whamlet|
    <span .label .label-default title="#{fullDuration}">
      #{shortDuration}
  |]
  where
    seconds = fromIntegral $ n `mod` 60
    minutes = fromIntegral $ n `div` 60 `mod` 60
    hours   = fromIntegral $ n `div` (60 * 60) `mod` 24
    days    = fromIntegral $ n `div` (24 * 60 * 60) `mod` 7
    weeks   = fromIntegral $ n `div` (7 * 24 * 60 * 60)

wSubmissionStatus :: SubmissionStatus -> Widget
wSubmissionStatus s = do
  [whamlet|
    $case s
      $of SubmissionSubmitted
        <span .label .label-default>_{MsgSubmissionSubmitted}
      $of SubmissionInvalid
        <span .label .label-danger>_{MsgSubmissionInvalid}
      $of SubmissionCompileError
        <span .label .label-danger>_{MsgSubmissionCompileError}
      $of SubmissionTestsFailed
        <span .label .label-danger>_{MsgSubmissionTestsFailed}
      $of SubmissionTestsPassed
        <span .label .label-warning>_{MsgSubmissionTestsPassed}
      $of SubmissionInReview
        <span .label .label-info>_{MsgSubmissionInReview}
      $of SubmissionRejected
        <span .label .label-danger>_{MsgSubmissionRejected}
      $of SubmissionAccepted
        <span .label .label-success>_{MsgSubmissionAccepted}
      $of SubmissionErrored
        <span .label .label-default>_{MsgSubmissionErrored}
  |]

wAchievement :: Achievement -> Bool -> Widget
wAchievement achievement withPopover = do
  mr <- getMessageRender
  descriptionHtml <- handlerToWidget $ noLayout $ wAchievementDescription achievement
  let
    predefined = achievementPredefined achievement
    name =
      case achievementCustomName achievement of
        Just n  -> n
        Nothing -> mr $ maybe MsgUntitledAchievement achievementPredefinedMsg predefined
  [whamlet|
    $if withPopover
      <button class="btn btn-trophy #{typeClass}" data-toggle="popover" data-trigger="hover" data-placement="bottom" data-content="#{renderHtml $ descriptionHtml}">
        #{name}
    $else
      <span class="trophy #{typeClass}">
        #{name}
  |]
  where
    typeClass :: Text
    typeClass =
      case achievementType achievement of
        AchievementGold   -> "trophy-gold"
        AchievementSilver -> "trophy-silver"
        AchievementBronze -> "trophy-bronze"
        AchievementSecret -> "trophy-secret"

wAchievementDescription :: Achievement -> Widget
wAchievementDescription achievement = do
  mr <- getMessageRender
  flavourMarkup $
    case achievementCustomDescription achievement of
      Just d  -> d
      Nothing -> mr $ maybe MsgAchievementNoDescription achievementPredefinedDescriptionMsg predefined
  where
    predefined = achievementPredefined achievement

flavourMarkup :: Text -> Widget
flavourMarkup s = do
  let
    widgets = Map.fromList $ map (id &&& wSubmissionStatus) statuses
    ws = Text.words s
  forM_ ws $ \w ->
    case w of
      "#accepted" -> widgets Map.! SubmissionAccepted
      _ -> toWidget $ toHtml $ w <> " "
  where
    statuses = [minBound..maxBound]

