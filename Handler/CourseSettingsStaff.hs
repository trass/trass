module Handler.CourseSettingsStaff where

import Import
import Yesod.Auth
import Handler.CourseSettings
import UserRole
import UtilsDB

getCourseSettingsStaffR :: Text -> Handler Html
getCourseSettingsStaffR cname = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  teachers   <- runDB $ selectStaff cid RoleTeacher
  assistants <- runDB $ selectStaff cid RoleAssistant
  courseSettingsLayout cname "staff" $(widgetFile "course/settings/staff")
