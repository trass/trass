module Handler.CourseStudentCoursePoints where

import Import
import Yesod.Auth
import Control.Monad

import Data.Time
import qualified Data.List as List

import Handler.CourseStudent
import Handler.CourseAssignment (wAssignmentLink)
import Handler.CourseSection (wSectionLink)

import SubmissionStatus
import Utils
import UtilsDB

getCourseStudentCoursePointsR :: Text -> UserId -> Handler Html
getCourseStudentCoursePointsR cname uid = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cname
  authId <- requireAuthId

  totalEvents <- runDB $ getStudentEventsCount courseId uid
  (pageNo, totalPages) <- pagerInfo totalEvents perPage

  events <- runDB $ getStudentEvents perPage pageNo courseId uid

  when (uid == authId) $ do
    runDB $ updateWhere
      [EventId <-. map (\(Entity eid _, _, _, _, _, _, _) -> eid) events]
      [EventIsRead =. True]

  extraPoints <- runDB $ selectList [ExtraPointsCourse ==. courseId] []
  let
    (bonuses, penalties) = List.partition ((>= 0) . extraPointsPoints . entityVal) extraPoints

  now <- liftIO getCurrentTime
  courseStudentLayout cname uid "points" $ do
    $(widgetFile "course/student/events")

  where
    perPage = 20

postCourseStudentCoursePointsR :: Text -> UserId -> Handler Html
postCourseStudentCoursePointsR = error "Not yet implemented: postCourseStudentCoursePointsR"

