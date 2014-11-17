module Handler.CourseMessages where

import Import
import Yesod.Auth
import Data.Time
import UserRole
import Utils
import UtilsDB

getCourseMessagesR :: Text -> Handler Html
getCourseMessagesR cname = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  role <- getUserRole cid authId
  case role of
    RoleStudent -> redirect $ CourseStudentConversationR cname authId
    RoleAssistant -> notFound
    RoleTeacher -> do
      now <- liftIO getCurrentTime
      conversations <- runDB $ selectConversations cid authId

      defaultLayout $ do
        $(widgetFile "teacher/messages")

