module Handler.SettingsCourses where

import Import
import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Handler.Settings
import Yesod.Auth

getSettingsCoursesR :: Handler Html
getSettingsCoursesR = do
  authId <- requireAuthId
  courseRoles <- runDB $ selectList [RoleUser ==. authId] []
  courses     <- runDB $ selectList ([CourseOwner ==. authId] ||. [CourseIdent <-. map (roleCourse . entityVal) courseRoles]) []
  let
    roles = Map.fromList $ map ((roleCourse &&& roleRole) . entityVal) courseRoles
    settingsTab = tabCourses authId roles courses
  defaultLayout $ do
    $(widgetFile "settings")
  where
    tabName = "courses"
