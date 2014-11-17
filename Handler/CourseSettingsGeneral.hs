module Handler.CourseSettingsGeneral where

import Import
import Handler.CourseSettings

getCourseSettingsGeneralR :: Text -> Handler Html
getCourseSettingsGeneralR cname = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  courseSettingsLayout cname "general" $(widgetFile "course/settings/general")
