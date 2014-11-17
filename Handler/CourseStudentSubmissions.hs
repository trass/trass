module Handler.CourseStudentSubmissions where

import Import
import Data.Time

import Handler.CourseStudent
import Handler.CourseAssignment (assignmentR)
import Handler.CourseSection (sectionR)

import Utils
import UtilsDB

getCourseStudentSubmissionsR :: Text -> UserId -> Handler Html
getCourseStudentSubmissionsR cname uid = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cname

  totalSubmissions <- runDB $ getSubmissionsCountByStudent courseId uid
  (pageNo, totalPages) <- pagerInfo totalSubmissions perPage

  submissions <- runDB $ getSubmissionsByStudent perPage pageNo courseId uid

  now <- liftIO getCurrentTime
  courseStudentLayout cname uid "submissions" $ do
    $(widgetFile "course/student/submissions")
  where
    perPage = 20

