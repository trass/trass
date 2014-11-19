module Handler.CourseSubmission where

import Import
import Yesod.Auth
import Control.Monad
import Data.Time
import UserRole
import SubmissionStatus
import Utils

getCourseSubmissionR :: Text -> SubmissionId -> Handler Html
getCourseSubmissionR cname sid = redirect $ CourseSubmissionSolutionR cname sid

courseSubmissionLayout :: Text -> SubmissionId -> Text -> UTCTime -> Widget -> Handler Html
courseSubmissionLayout cname sid tabName now tab = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  userRole <- getUserRole cid authId
  submission <- runDB $ get404 sid

  when (isStudent userRole && submissionAuthor submission /= authId) $
    notFound

  let
    uid = submissionAuthor submission
    aid = submissionAssignment submission
    status = submissionStatus submission
    updatedAt = submissionUpdatedAt submission

  assignment <- runDB $ get404 aid
  section <- runDB $ get404 $ assignmentSection assignment
  Entity _ student <- runDB $ getBy404 $ UniqueProfile uid

  defaultLayout $ do
    $(widgetFile "course/submission")

  where
    isTabSolution = tabName == "solution"
    isTabHistory  = tabName == "history"

