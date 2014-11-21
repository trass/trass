module Handler.Course.Settings.Gamification where

import Import
import Handler.Course.Settings

getCourseSettingsGamificationR :: Text -> Handler Html
getCourseSettingsGamificationR cname =
  courseSettingsLayout cname "gamification" $(widgetFile "course/settings/gamification")
