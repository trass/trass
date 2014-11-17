module Handler.CourseStudent where

import Import
import Yesod.Auth
import Control.Monad

import UserRole
import Utils
import UtilsDB

getCourseStudentR :: Text -> UserId -> Handler Html
getCourseStudentR cname uid = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cid) mauthId
  let isOtherStudent = isStudent userRole && Just uid /= mauthId

  redirect $
    if isOtherStudent
      then CourseStudentAchievementsR cname uid
      else CourseStudentCoursePointsR cname uid

courseStudentLayout :: ToWidget App a => Text -> UserId -> Text -> a -> Handler Html
courseStudentLayout cname uid tabName tab = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  Entity _ profile  <- runDB $ getBy404 $ UniqueProfile uid

  studentRole <- getUserRole cid uid
  when (not $ isStudent studentRole) $ notFound

  mauthId <-
    if isTabAchievements
      then maybeAuthId
      else Just <$> requireAuthId

  userRole <- maybe (return RoleStudent) (getUserRole cid) mauthId

  let isOtherStudent = isStudent userRole && Just uid /= mauthId
  when (isOtherStudent && not isTabAchievements) $ do
    notFound

  studentGroup <- runDB $ getStudentGroup cid uid

  defaultLayout $ do
    $(widgetFile "course/student")
  where
    isTabAchievements = tabName == "achievements"
    isTabCoursePoints = tabName == "points"
    isTabRating       = tabName == "rating"
    isTabSubmissions  = tabName == "submissions"
    isTabAssignments  = tabName == "assignments"
    isTabConversation = tabName == "conversation"

