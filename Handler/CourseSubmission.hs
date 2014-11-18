module Handler.CourseSubmission where

import Import
import Yesod.Auth
import Data.Time
import UserRole
import SubmissionStatus
import Utils

getCourseSubmissionR :: Text -> SubmissionId -> Handler Html
getCourseSubmissionR cname sid = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  userRole <- getUserRole cid authId
  submission <- runDB $ get404 sid

  let
    uid = submissionAuthor submission
    aid = submissionAssignment submission
    status = submissionStatus submission
    updatedAt = submissionUpdatedAt submission

  assignment <- runDB $ get404 aid
  section <- runDB $ get404 $ assignmentSection assignment
  Entity _ student <- runDB $ getBy404 $ UniqueProfile uid

  now <- liftIO getCurrentTime
  defaultLayout $ do
    $(widgetFile "course/submission")

postCourseSubmissionR :: Text -> SubmissionId -> Handler Html
postCourseSubmissionR = error "Not yet implemented: postCourseSubmissionR"
