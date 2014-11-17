module Handler.CourseStudent where

import Import
import Yesod.Auth
import Control.Monad

import UserRole
import Utils

getCourseStudentR :: Text -> UserId -> Handler Html
getCourseStudentR cid uid = redirect $ CourseStudentAchievementsR cid uid

courseStudentLayout :: ToWidget App a => Text -> UserId -> Text -> a -> Handler Html
courseStudentLayout cname uid tabName tab = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cname
  Entity _ profile  <- runDB $ getBy404 $ UniqueProfile uid

  studentRole <- getUserRole cname uid
  when (not $ isStudent studentRole) $ notFound

  mauthId <-
    if tabName == "achievements"
      then maybeAuthId
      else Just <$> requireAuthId

  userRole <- maybe (return RoleStudent) (getUserRole cname) mauthId

  let isOtherStudent = isStudent userRole && Just uid /= mauthId
  when (isOtherStudent && tabName /= "achievement") $ do
    notFound

  defaultLayout $ do
    $(widgetFile "course/student")
  where
    isTabAchievements = tabName == "achievements"
    isTabCoursePoints = tabName == "points"
    isTabRating       = tabName == "rating"
    isTabSubmissions  = tabName == "submissions"
    isTabAssignments  = tabName == "assignments"
    isTabConversation = tabName == "conversation"

