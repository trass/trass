module Handler.CourseSubmissions where

import Import
import Yesod.Auth
import Handler.CourseSubmissionsByStatus
import SubmissionStatus
import UserRole

getCourseSubmissionsR :: Text -> Handler Html
getCourseSubmissionsR cname = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  userRole <- getUserRole cid authId

  case userRole of
    RoleStudent -> redirect $ CourseStudentSubmissionsR cname authId
    _ -> redirect $ CourseSubmissionsByStatusR cname SubmissionTestsPassed
