module Handler.CourseStudentCoursePoints where

import Import
import Yesod.Auth
import Control.Monad

import Data.Time
import qualified Data.Text as Text

import Handler.CourseStudent
import Handler.CourseAssignment (assignmentR)
import Handler.CourseSection (sectionR)

import Utils
import UtilsDB

getCourseStudentCoursePointsR :: Text -> UserId -> Handler Html
getCourseStudentCoursePointsR cname uid = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cname
  authId <- requireAuthId

  totalEvents <- runDB $ getStudentCoursePointsCount courseId uid
  (pageNo, totalPages) <- pagerInfo totalEvents perPage

  coursePointsEvents <- runDB $ getStudentCoursePoints perPage pageNo courseId uid

  when (uid == authId) $ do
    runDB $ updateWhere
      [CoursePointsId <-. map (\(Entity eid _, _, _, _) -> eid) coursePointsEvents]
      [CoursePointsIsRead =. True]

  now <- liftIO getCurrentTime
  courseStudentLayout cname uid "points" $ do
    $(widgetFile "course/student/events")

  where
    perPage = 20

postCourseStudentCoursePointsR :: Text -> UserId -> Handler Html
postCourseStudentCoursePointsR = error "Not yet implemented: postCourseStudentCoursePointsR"

