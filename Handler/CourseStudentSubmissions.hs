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

  now <- liftIO getCurrentTime
  courseStudentLayout cname uid "submissions" $ do
    $(widgetFile "course/student/submissions")

