module Handler.CourseSubmissions where

import Import
import Yesod.Auth
import Handler.CourseSubmissionsByStatus
import SubmissionStatus
import UserRole

getCourseSubmissionsR :: Text -> Handler Html
getCourseSubmissionsR cid = do
  authId <- requireAuthId
  userRole <- getUserRole cid authId

  case userRole of
    RoleStudent -> redirect $ CourseStudentSubmissionsR cid authId
    _ -> redirect $ CourseSubmissionsByStatusR cid SubmissionTestsPassed
