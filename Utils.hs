module Utils where

import Import
import Yesod.Core
import Data.Maybe
import Data.Time
import Data.Int
import Text.Read (readMaybe)
import qualified Data.Text as Text
import AssignmentAction
import UserRole

data Ago
  = AgoJustNow
  | AgoMinutes Int
  | AgoHours Int
  | AgoDays Int
  | AgoWeeks Int
  | AgoMonths Int
  | AgoYears Int
  deriving (Eq, Show, Read)

agoMsg :: Ago -> AppMessage
agoMsg AgoJustNow     = MsgTimeAgoJustNow
agoMsg (AgoMinutes n) = MsgTimeAgoMinutes n
agoMsg (AgoHours   n) = MsgTimeAgoHours n
agoMsg (AgoDays    n) = MsgTimeAgoDays n
agoMsg (AgoWeeks   n) = MsgTimeAgoWeeks n
agoMsg (AgoMonths  n) = MsgTimeAgoMonths n
agoMsg (AgoYears   n) = MsgTimeAgoYears n

ago :: UTCTime -> UTCTime -> Ago
ago t now
  | minutes == 0  = AgoJustNow
  | minutes <  5  = AgoMinutes minutes
  | hours == 0    = AgoMinutes (5 * (minutes `div` 5))
  | days == 0     = AgoHours hours
  | weeks == 0    = AgoDays days
  | months == 0   = AgoWeeks weeks
  | years == 0    = AgoMonths months
  | otherwise     = AgoYears years
  where
    seconds = floor (diffUTCTime now t)
    minutes = seconds `div` 60
    hours   = minutes `div` 60
    days    = hours `div` 24
    weeks   = days `div` 7
    months  = days `div` 30
    years   = days `div` 365

wAgo :: UTCTime -> UTCTime -> Widget
wAgo dt now = do
  [whamlet|
    <small .text-help title=#{formatTimeFull dt}>
      _{agoMsg $ ago dt now}
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

pagerInfo :: (Integral a, Integral b) => a -> b -> Handler (Int64, Int64)
pagerInfo total perPage = do
  pageNoStr <- lookupGetParam "page"

  let totalPages = ceiling (fromIntegral total / fromIntegral perPage)

  pageNo <-
    case pageNoStr >>= readMaybe . Text.unpack of
      Nothing -> return 1
      Just n | 1 <= n && n <= totalPages -> return n
      _ -> notFound

  return (pageNo, totalPages)

wPager :: Int64 -> Int64 -> Route App -> Widget
wPager pageNo totalPages route = do
  [whamlet|
    $if totalPages > 1
      <div .text-center>
        <ul .pagination>
          $if pageNo == 1
            <li .disabled>
              <span>&laquo;
          $else
            <li>
              <a href="@?{pageR $ pageNo - 1}">
                &laquo;

          $forall n <- enumFromTo 1 totalPages
            $if n == pageNo
              <li .active>
                <span>
                  #{show n}
                  <span .sr-only>(current)
            $else
              <li>
                <a href="@?{pageR n}">
                  #{show n}

          $if pageNo == totalPages
            <li .disabled>
              <span>&raquo;
          $else
            <li>
              <a href="@?{pageR $ pageNo + 1}">
                &raquo;
  |]
  where
    pageR n = (route, [("page", Text.pack $ show $ n)])

sectionR :: Text -> Section -> Route App
sectionR cname s = CourseSectionR cname $ Text.splitOn "/" (sectionIdent s)

wSectionLink :: Text -> Section -> Widget
wSectionLink cname section = do
  [whamlet|
    <a href="@{sectionR cname section}">#{sectionTitle section}
  |]

assignmentR :: Text -> Assignment -> Section -> Route App
assignmentR cname a s = CourseAssignmentR cname (Text.splitOn "/" (sectionIdent s) ++ [assignmentIdent a])

wAssignmentLink :: Text -> Assignment -> Section -> Widget
wAssignmentLink cname assignment section = do
  [whamlet|
    <a href="@{assignmentR cname assignment section}">#{assignmentTitle assignment}
  |]

wAssignmentsInfo :: Bool -> UserRole -> Bool -> UTCTime -> Maybe Int -> Maybe Int64 -> Maybe UTCTime -> Maybe UTCTime -> Widget
wAssignmentsInfo isSection userRole locked now mpoints mduration mstart mend = do
  [whamlet|
      <ul .list-group>
        $maybe points <- mpoints
          <li .list-group-item>
            <span .pull-right>
              $if locked
                <i .fa .fa-lg .fa-fw .fa-lock>
              $if isStudent userRole
                <span .label :False:.label-success :True:.label-warning>
                  10 / #{points}
              $else
                <span .label :isNothing mstart:.label-default :isJust mstart:.label-success>
                  #{points}
            _{MsgCoursePoints}
        $maybe start <- mstart
          $if isTeacher userRole
            <li .list-group-item>
              <span .pull-right>
                <span .label-date .label .label-default title="#{formatTimeFull start}">
                  #{formatTimeFull start}
              _{choose MsgSectionStartDate MsgAssignmentStartDate}
          <li .list-group-item>
            $maybe end <- mend
              <span .pull-right>
                <span .label-date .label :inFuture end:.label-success :inPast end:.label-danger title="#{formatTimeFull end}">
                  #{formatTimeFull end}
            $nothing
              <span .pull-right>
                <span .label .label-success>
                  _{MsgNoDeadline}
            _{choose MsgSectionEndDate MsgAssignmentEndDate}
        $nothing
          $if isTeacher userRole
            $maybe duration <- mduration
              <li .list-group-item>
                <span .pull-right>
                  ^{wDuration duration}
                _{choose MsgSectionDuration MsgAssignmentDuration}
          <li .list-group-item>
            _{choose MsgSectionNotStartedYet MsgAssignmentNotStartedYet}
  |]
  where
    inFuture dt = now < dt
    inPast = not . inFuture
    choose msg msg' = if isSection then msg else msg'

wAssignmentsManagePanel :: Bool -> Bool -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> (AssignmentAction -> Route App) -> Widget
wAssignmentsManagePanel isSection locked now mstart mend actionR = do
  [whamlet|
    <div .panel .panel-danger>
      <div .panel-heading>
        <h3 .panel-title>_{MsgAssignmentManagePanelTitle}

      <div .manage-assignment .list-group>
        $if isJust mend
          <a href="javascript:void(0)" .list-group-item data-toggle="popover" data-trigger="hover" data-placement="top" data-content="_{choose MsgSectionManageExtraDayDescription MsgAssignmentManageExtraDayDescription}">
            <i class="fa fa-plus-square fa-lg fa-fw text-danger">
            _{choose MsgSectionManageExtraDay MsgAssignmentManageExtraDay}
            <form role="form" method="post" action=@{actionR AssignmentExtraDay}>
          <a href="javascript:void(0)" .list-group-item data-toggle="popover" data-trigger="hover" data-placement="top" data-content="_{choose MsgSectionManageSkipDayDescription MsgAssignmentManageSkipDayDescription}">
            <i class="fa fa-minus-square fa-lg fa-fw text-danger">
            _{choose MsgSectionManageSkipDay MsgAssignmentManageSkipDay}
            <form role="form" method="post" action=@{actionR AssignmentSkipDay}>

        $if isJust mstart
          $if or [isNothing mend, Just now < mend]
            <a href="javascript:void(0)" .list-group-item data-toggle="popover" data-trigger="hover" data-placement="top" data-content="_{choose MsgSectionManageStopDescription MsgAssignmentManageStopDescription}">
              <i class="fa fa-stop fa-lg fa-fw text-danger">
              _{choose MsgSectionManageStop MsgAssignmentManageStop}
              <form role="form" method="post" action=@{actionR AssignmentStop}>
        $else
          <a href="javascript:void(0)" .list-group-item data-toggle="popover" data-trigger="hover" data-placement="top" data-content="_{choose MsgSectionManageStartDescription MsgAssignmentManageStartDescription}">
            <i class="fa fa-play fa-lg fa-fw text-danger">
            _{choose MsgSectionManageStart MsgAssignmentManageStart}
            <form role="form" method="post" action=@{actionR AssignmentStart}>

        $if locked
          <a href="javascript:void(0)" .list-group-item data-toggle="popover" data-trigger="hover" data-placement="top" data-content="_{choose MsgSectionManageUnlockDescription MsgAssignmentManageUnlockDescription}">
            <i class="fa fa-unlock fa-lg fa-fw text-danger">
            _{choose MsgSectionManageUnlock MsgAssignmentManageUnlock}
            <form role="form" method="post" action=@{actionR AssignmentUnlock}>
        $else
          <a href="javascript:void(0)" .list-group-item data-toggle="popover" data-trigger="hover" data-placement="top" data-content="_{choose MsgSectionManageLockDescription MsgAssignmentManageLockDescription}">
            <i class="fa fa-lock fa-lg fa-fw text-danger">
            _{choose MsgSectionManageLock MsgAssignmentManageLock}
            <form role="form" method="post" action=@{actionR AssignmentLock}>

        <a href="javascript:void(0)" .list-group-item data-toggle="popover" data-trigger="hover" data-placement="top" data-content="_{choose MsgSectionManageResetDescription MsgAssignmentManageResetDescription}">
          <i class="fa fa-refresh fa-lg fa-fw text-danger">
          _{choose MsgSectionManageReset MsgAssignmentManageReset}
            <form role="form" method="post" action=@{actionR AssignmentReset}>
  |]
  where
    choose msg msg' = if isSection then msg else msg'

wStudentCoursePoints :: CourseId -> UserId -> Widget
wStudentCoursePoints _ _ = [whamlet| |]

wStudentAchievementsTotal :: CourseId -> UserId -> Widget
wStudentAchievementsTotal _ _ = [whamlet| |]

