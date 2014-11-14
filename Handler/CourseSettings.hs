module Handler.CourseSettings where

import Import
import Control.Monad (when)
import Yesod.Auth
import UserRole

courseSettingsLayout :: ToWidget App a => Text -> Text -> a -> Handler Html
courseSettingsLayout cid tabName settingsTab = do
  authId <- requireAuthId
  userRole <- getUserRole cid authId

  Entity _ course <- runDB $ getBy404 $ UniqueCourse cid
  let isCourseOwner = courseOwner course == authId
  when (not isCourseOwner) $ notFound

  section <- runDB $ get404 $ courseRootSection course
  let
    isCoursePreview   = False
    isCourseGraphs    = False
    isCourseSettings  = True
    courseHeader = $(widgetFile "course/header")

  defaultLayout $(widgetFile "course/settings")

getCourseSettingsR :: Text -> Handler Html
getCourseSettingsR = redirect . CourseSettingsGeneralR

isTabGeneral :: Text -> Bool
isTabGeneral = (== "general")

isTabStaff :: Text -> Bool
isTabStaff = (== "staff")

isTabGamification :: Text -> Bool
isTabGamification = (== "gamification")

