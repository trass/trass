module Utils where

import Import
import Yesod.Core
import Control.Monad
import Control.Arrow ((&&&))
import Data.Maybe
import Data.Time
import Data.Int
import Text.Read (readMaybe)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map as Map
import SubmissionStatus
import UserRole
import Achievement
import UtilsDB

import Text.Blaze.Html.Renderer.Text (renderHtml)

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

achievementTypeClass :: AchievementType -> Text
achievementTypeClass AchievementGold   = "trophy-gold"
achievementTypeClass AchievementSilver = "trophy-silver"
achievementTypeClass AchievementBronze = "trophy-bronze"
achievementTypeClass AchievementSecret = "trophy-secret"

wAchievementType :: AchievementType -> Widget
wAchievementType t = do
  [whamlet|
    <span class="trophy #{achievementTypeClass t}">
  |]

wAchievement :: Achievement -> Bool -> Widget
wAchievement achievement withPopover = do
  descriptionHtml <- handlerToWidget $ noLayout $ wAchievementDescription achievement
  let name = wAchievementName achievement
  [whamlet|
    $if withPopover
      <button class="btn btn-trophy #{typeClass}" data-toggle="popover" data-trigger="hover" data-placement="bottom" data-content="#{renderHtml $ descriptionHtml}">
        &nbsp;^{name}
    $else
      <span class="trophy #{typeClass}">
        &nbsp;^{name}
  |]
  where
    typeClass = achievementTypeClass $ achievementType achievement

wAchievementName :: Achievement -> Widget
wAchievementName achievement = do
  [whamlet|
    $maybe customName <- achievementCustomName achievement
      #{customName}
    $nothing
      $maybe predefined <- achievementPredefined achievement
        _{achievementPredefinedMsg predefined}
      $nothing
        _{MsgUntitledAchievement}
  |]

wAchievementDescription :: Achievement -> Widget
wAchievementDescription achievement = do
  mr <- getMessageRender
  flavourMarkup $
    case achievementCustomDescription achievement of
      Just d  -> d
      Nothing -> mr $ maybe MsgAchievementNoDescription achievementPredefinedDescriptionMsg predefined
  where
    predefined = achievementPredefined achievement

wStudentCoursePoints :: CourseId -> UserId -> Widget
wStudentCoursePoints cid uid = do
  points <- handlerToWidget $ runDB $ getStudentCoursePointsSum cid uid
  [whamlet|
    <span .label .label-success title="_{MsgCoursePoints}">
      #{points}
  |]

wStudentAchievementsTotal :: CourseId -> UserId -> Widget
wStudentAchievementsTotal cid uid = do
  totals <- handlerToWidget $ runDB $ getStudentAchievementsTotal cid uid
  let
    typeTotal t = fromMaybe 0 $ lookup t totals
    goldAchievementsTotal     = typeTotal AchievementGold
    silverAchievementsTotal   = typeTotal AchievementSilver
    bronzeAchievementsTotal   = typeTotal AchievementBronze
    secretAchievementsTotal   = typeTotal AchievementSecret
    hasNotSecretAchievements  = any (> 0) [goldAchievementsTotal, silverAchievementsTotal, bronzeAchievementsTotal]
  [whamlet|
    <span .text-nowrap>
      $if hasNotSecretAchievements
        <span .trophy>
          $if goldAchievementsTotal > 0
            <span .trophy-gold title="_{MsgGoldAchievements}"> #{goldAchievementsTotal}
          $if silverAchievementsTotal > 0
            <span .trophy-silver title="_{MsgSilverAchievements}"> #{silverAchievementsTotal}
          $if bronzeAchievementsTotal > 0
            <span .trophy-bronze title="_{MsgBronzeAchievements}"> #{bronzeAchievementsTotal}
      $if secretAchievementsTotal > 0
        <span .trophy .trophy-secret title="_{MsgSecretAchievements}"> #{secretAchievementsTotal}
  |]

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

wExtraPoints :: ExtraPoints -> Maybe Assignment -> Widget
wExtraPoints ep ma = do
  [whamlet|
    $if points >= 0
      <span .label .label-success>
        + #{show points}
        $if percents
           %
    $else
      <span .label .label-danger>
        – #{show $ abs points}
        $if percents
           %
  |]
  where
    percents = extraPointsPercents ep && isNothing ma
    epoints = extraPointsPoints ep
    apoints = ma >>= assignmentPoints
    withPercents n = n + (n * epoints `div` 100)
    points =
      if extraPointsPercents ep && isJust ma
        then maybe 0 withPercents apoints
        else epoints

wExtraPointsDescription :: ExtraPoints -> Widget
wExtraPointsDescription ep = do
  [whamlet|
    $maybe customName <- extraPointsCustomName ep
      #{customName}
    $nothing
      $maybe predefined <- extraPointsPredefined ep
        _{extraPointsPredefinedMsg predefined}
      $nothing
        $if extraPointsPoints ep >= 0
          unknown bonus
        $else
          unknown penalty
  |]

wExtraPointsPanels :: CourseId -> Widget
wExtraPointsPanels cid = do
  extraPoints <- handlerToWidget $ runDB $ selectList [ExtraPointsCourse ==. cid] []
  let (bonuses, penalties) = List.partition ((>= 0) . extraPointsPoints . entityVal) extraPoints
  [whamlet|
    <div class="row">
      <div class="col-md-6">
        <div class="panel panel-success">
          <div class="panel-heading">
            <h3 class="panel-title">Bonuses
          $if null bonuses
            <div .panel-body>
              No bonuses.
          $else
            <div class="list-group">
              $forall Entity _ bonus <- bonuses
                <a href="#" class="list-group-item">
                  <span .pull-right>
                    ^{wExtraPoints bonus Nothing}
                  ^{wExtraPointsDescription bonus}

      <div class="col-md-6">
        <div class="panel panel-danger">
          <div class="panel-heading">
            <h3 class="panel-title">Penalties
          $if null penalties
            <div .panel-body>
              No penalties.
          $else
            <div class="list-group">
              $forall Entity _ penalty <- penalties
                <a href="#" class="list-group-item">
                  <span .pull-right>
                    ^{wExtraPoints penalty Nothing}
                  ^{wExtraPointsDescription penalty}
  |]

wCourseSubmissions :: Text -> UserId -> UTCTime -> Maybe UserId -> Maybe SubmissionStatus -> Maybe AssignmentId -> Route App -> Widget
wCourseSubmissions cname authId now muid mstatus maid route = do
  Entity cid _ <- handlerToWidget $ runDB $ getBy404 $ UniqueCourse cname
  totalSubmissions <- handlerToWidget $ runDB $ getCourseSubmissionsCount cid muid mstatus maid
  (pageNo, totalPages) <- handlerToWidget $ pagerInfo totalSubmissions perPage
  submissions <- handlerToWidget $ runDB $ getCourseSubmissions perPage pageNo cid muid mstatus maid

  let
    assignmentIds = map submissionAssignment submissions
    authorIds     = map submissionAuthor     submissions

  assignments <- handlerToWidget $ runDB $
    case maid of
      Nothing   -> selectList [AssignmentId <-. assignmentIds] []
      Just aid  -> (:[]) . Entity aid <$> get404 aid
  authors <- handlerToWidget $ runDB $
    case muid of
      Nothing   -> selectList [ProfileUser  <-. authorIds] []
      Just uid  -> (:[]) <$> getBy404 (UniqueProfile uid)
  let
    sectionIds    = map (assignmentSection . entityVal) assignments

  sections <- handlerToWidget $ runDB $ selectList [SectionId <-. sectionIds] []
  let
    getEntity k f es s = Map.fromList (map (k &&& entityVal) es) Map.! f s
    getAssignment = getEntity entityKey submissionAssignment assignments
    getSection    = getEntity entityKey assignmentSection sections
    getAuthor     = getEntity (profileUser . entityVal) submissionAuthor authors
    submissions'  = map (\s -> let a = getAssignment s in (s, a, getSection a, getAuthor s)) submissions
  [whamlet|
    <div .panel .panel-default>
      <div .panel-heading>
        <h3 .panel-title>
          Submissions
      $if null submissions
        <div .panel-body>
          _{MsgNoSubmissions}
      $else
        <table class="table table-condensed">
          <tbody>
            $forall (submission, assignment, section, author) <- submissions'
              <tr>
                <td class="text-nowrap text-center">
                  <a href="#" title="View">
                    <i class="fa fa-eye fa-lg fa-fw">
                  <a href="#" title="Download">
                    <i class="fa fa-download fa-lg fa-fw">
                $if isNothing maid
                  <td>
                    ^{wAssignmentLink cname assignment section}
                  <td class="hidden-xs hidden-sm">
                    <small>
                      ^{wSectionLink cname section}
                $if isNothing muid
                  <td>
                    <small>
                      <a href="@{CourseStudentR cname $ profileUser author}">
                        <span class="text-nowrap">
                          ^{wUserName RoleStudent (Just author) (Just authId)}
                $if isNothing mstatus
                  <td>
                    ^{wSubmissionStatus $ submissionStatus submission}
                <td>
                  ^{wAgo (submissionUpdatedAt submission) now}

    ^{wPager pageNo totalPages route}
  |]
  where
    perPage = 20

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

