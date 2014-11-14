module Handler.CourseMessages where

import Import
import Yesod.Auth
import Handler.CourseMessagesStudent
import Data.Time
import UserRole
import Utils
import UtilsDB

getCourseMessagesR :: Text -> Handler Html
getCourseMessagesR cid = do
  authId <- requireAuthId
  role <- getUserRole cid authId
  case role of
    RoleStudent -> getCourseMessagesStudentR cid authId
    RoleAssistant -> notFound
    RoleTeacher -> do
      now <- liftIO getCurrentTime
      conversations <- runDB $ selectConversations cid authId

      defaultLayout $ do
        $(widgetFile "teacher/messages")

postCourseMessagesR :: Text -> Handler Html
postCourseMessagesR cid = do
  authId <- requireAuthId
  role <- getUserRole cid authId
  case role of
    RoleStudent -> postCourseMessagesStudentR cid authId
    _ -> notFound
