module Handler.Course.Settings.General where

import Import
import Handler.Course.Settings

getCourseSettingsGeneralR :: Text -> Handler Html
getCourseSettingsGeneralR cname = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  courseSettingsLayout cname "general" $(widgetFile "course/settings/general")
