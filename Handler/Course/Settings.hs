module Handler.Course.Settings where

import Import
import qualified Data.List as List
import Control.Monad (when)
import Yesod.Auth
import UserRole

courseSettingsLayout :: ToWidget App a => Text -> Text -> a -> Handler Html
courseSettingsLayout cname tabName settingsTab = do
  authId <- requireAuthId
  Entity cid course <- runDB $ getBy404 $ UniqueCourse cname
  userRole <- getUserRole cid authId

  let isCourseOwner = courseOwner course == authId
  when (not isCourseOwner) $ notFound

  section <- runDB $ get404 $ courseRootSection course
  let
    crumbs :: [([Text], Text)]
    crumbs = []
    isCoursePreview   = False
    isCourseGraphs    = False
    isCourseSettings  = True
    headerTitle       = sectionTitle section
    headerSummary     = sectionSummary section
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

