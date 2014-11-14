module Handler.CourseSettingsGamification where

import Import
import Handler.CourseSettings

getCourseSettingsGamificationR :: Text -> Handler Html
getCourseSettingsGamificationR cid =
  courseSettingsLayout cid "gamification" $(widgetFile "course/settings/gamification")
