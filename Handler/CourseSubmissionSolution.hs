module Handler.CourseSubmissionSolution where

import Import
import Handler.CourseSubmission
import Data.Time

getCourseSubmissionSolutionR :: Text -> SubmissionId -> Handler Html
getCourseSubmissionSolutionR cname sid = do
  now <- liftIO getCurrentTime
  courseSubmissionLayout cname sid "solution" now $ do
    $(widgetFile "course/submission/solution")

