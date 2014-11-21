module Handler.Settings.Courses where

import Import
import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Handler.Settings
import Yesod.Auth
import UtilsDB

getSettingsCoursesR :: Handler Html
getSettingsCoursesR = do
  authId <- requireAuthId
  courseRoles <- runDB $ selectList [RoleUser ==. authId] []
  courses     <- runDB $ getUserCourses authId
  let
    roles = Map.fromList $ map ((roleCourse &&& roleRole) . entityVal) courseRoles
    settingsTab = tabCourses authId roles courses
  defaultLayout $ do
    $(widgetFile "settings")
  where
    tabName = "courses"
