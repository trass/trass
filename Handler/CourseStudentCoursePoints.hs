module Handler.CourseStudentCoursePoints where

import Import
import Yesod.Auth
import Yesod.Core.Dispatch
import Control.Monad

import UserRole
import SubmissionStatus

import Data.Time
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Handler.CourseStudent
import Handler.CourseAssignment (assignmentR)
import Handler.CourseSection (sectionR)

import Utils
import UtilsDB

getCourseStudentCoursePointsR :: Text -> UserId -> Handler Html
getCourseStudentCoursePointsR cname uid = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cname
  Entity _ profile  <- runDB $ getBy404 $ UniqueProfile uid

  studentRole <- getUserRole cname uid
  when (not $ isStudent studentRole) $ notFound

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cname) mauthId

  let isOtherStudent = isStudent userRole && Just uid /= mauthId
  when isOtherStudent $ do
    notFound

  pageNoStr <- lookupGetParam "page"

  totalEvents <- runDB $ getStudentCoursePointsCount courseId uid
  let totalPages = ceiling (fromIntegral totalEvents / fromIntegral perPage)

  pageNo <-
    case pageNoStr >>= readMaybe . Text.unpack of
      Nothing -> return 1
      Just n | 1 <= n && n <= totalPages -> return n
      _ -> notFound

  coursePoints <- runDB $ getStudentCoursePointsSum courseId uid
  achievementTotals <- runDB $ getStudentAchievementsTotal courseId uid

  coursePointsEvents <- runDB $ getStudentCoursePoints perPage pageNo courseId uid

  when (Just uid == mauthId) $ do
    runDB $ updateWhere
      [CoursePointsId <-. map (\(Entity eid _, _, _, _) -> eid) coursePointsEvents]
      [CoursePointsIsRead =. True]

  now <- liftIO getCurrentTime
  let tab = $(widgetFile "course/student/events")

  defaultLayout $ do
    $(widgetFile "course/student")
  where
    nav :: Widget
    nav = $(widgetFile "course/student/nav")
    tabName = "points"
    perPage = 20
    pageR n = (CourseStudentCoursePointsR cname uid, [("page", Text.pack $ show $ n)])

postCourseStudentCoursePointsR :: Text -> UserId -> Handler Html
postCourseStudentCoursePointsR = error "Not yet implemented: postCourseStudentCoursePointsR"

