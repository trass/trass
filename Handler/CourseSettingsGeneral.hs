module Handler.CourseSettingsGeneral where

import Import
import Handler.CourseSettings

getCourseSettingsGeneralR :: Text -> Handler Html
getCourseSettingsGeneralR cid =
  courseSettingsLayout cid "general" $(widgetFile "course/settings/general")
