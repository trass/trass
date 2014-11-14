module Handler.CourseSettingsGeneral where

import Import
import Control.Monad
import Yesod.Auth
import Handler.CourseSettings
import UserRole

getCourseSettingsGeneralR :: Text -> Handler Html
getCourseSettingsGeneralR cid =
  courseSettingsLayout cid "general" $(widgetFile "course/settings/general")
