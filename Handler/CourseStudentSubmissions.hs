module Handler.CourseStudentSubmissions where

import Import
import Yesod.Auth
import Data.Time

import Handler.CourseStudent

import Utils
import UtilsDB

getCourseStudentSubmissionsR :: Text -> UserId -> Handler Html
getCourseStudentSubmissionsR cname uid = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname

  totalSubmissions <- runDB $ getSubmissionsCountByStudent cid uid
  (pageNo, totalPages) <- pagerInfo totalSubmissions perPage

  submissions <- runDB $ getSubmissionsByStudent perPage pageNo cid uid

  now <- liftIO getCurrentTime
  courseStudentLayout cname uid "submissions" $ do
    $(widgetFile "course/student/submissions")
  where
    perPage = 20

