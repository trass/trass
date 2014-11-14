module Handler.CourseSettingsStaff where

import Import
import Yesod.Auth
import Handler.CourseSettings
import UserRole
import UtilsDB

getCourseSettingsStaffR :: Text -> Handler Html
getCourseSettingsStaffR cid = do
  authId <- requireAuthId
  teachers   <- runDB $ selectStaff cid RoleTeacher
  assistants <- runDB $ selectStaff cid RoleAssistant
  courseSettingsLayout cid "staff" $(widgetFile "course/settings/staff")
