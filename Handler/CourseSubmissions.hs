module Handler.CourseSubmissions where

import Import
import Handler.CourseSubmissionsByStatus
import SubmissionStatus

getCourseSubmissionsR :: Text -> Handler Html
getCourseSubmissionsR cid = redirect $ CourseSubmissionsByStatusR cid SubmissionTestsPassed
