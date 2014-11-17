module Handler.CourseSettingsGamification where

import Import
import Handler.CourseSettings

getCourseSettingsGamificationR :: Text -> Handler Html
getCourseSettingsGamificationR cname =
  courseSettingsLayout cname "gamification" $(widgetFile "course/settings/gamification")
