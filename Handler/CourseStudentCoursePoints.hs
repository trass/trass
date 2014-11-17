module Handler.CourseStudentCoursePoints where

import Import
import Yesod.Auth
import Control.Monad

import Data.Time

import Handler.CourseStudent
import Handler.CourseAssignment (wAssignmentLink)
import Handler.CourseSection (wSectionLink)

import SubmissionStatus
import UserRole
import Utils
import UtilsDB

getCourseStudentCoursePointsR :: Text -> UserId -> Handler Html
getCourseStudentCoursePointsR cname uid = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  userRole <- getUserRole cid authId

  totalEvents <- runDB $ getStudentEventsCount cid uid
  (pageNo, totalPages) <- pagerInfo totalEvents perPage

  events <- runDB $ getStudentEvents perPage pageNo cid uid

  when (uid == authId) $ do
    runDB $ updateWhere
      [EventId <-. map (\(Entity eid _, _, _, _, _, _, _) -> eid) events]
      [EventIsRead =. True]

  now <- liftIO getCurrentTime
  courseStudentLayout cname uid "points" $ do
    $(widgetFile "course/student/events")

  where
    perPage = 20

postCourseStudentCoursePointsR :: Text -> UserId -> Handler Html
postCourseStudentCoursePointsR = error "Not yet implemented: postCourseStudentCoursePointsR"

