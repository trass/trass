module Handler.CourseSettingsStaff where

import Import
import Handler.CourseSettings

getCourseSettingsStaffR :: Text -> Handler Html
getCourseSettingsStaffR cid =
  courseSettingsLayout cid "staff" $(widgetFile "course/settings/staff")
