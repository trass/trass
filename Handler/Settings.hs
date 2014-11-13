module Handler.Settings where

import Import
import Data.Map (Map)
import qualified Data.Map as Map
import Yesod.Auth.Email (setpassR, forgotPasswordR)
import Data.Maybe
import UserRole

getSettingsR :: Handler Html
getSettingsR = redirect SettingsProfileR

tabProfile :: Profile -> Widget
tabProfile profile = $(widgetFile "settings/profile")

tabAccount :: Widget
tabAccount = $(widgetFile "settings/account")

tabStudentNotifications :: Widget
tabStudentNotifications = $(widgetFile "student/settings/notifications")

tabStudentCourses :: UserId -> Map Text UserRole -> [Entity Course] -> Widget
tabStudentCourses authId roles courses = $(widgetFile "student/settings/courses")
  where
    roleIn cid = roles Map.! cid

isTabProfile :: Text -> Bool
isTabProfile = (== "profile")

isTabAccount :: Text -> Bool
isTabAccount = (== "account")

isTabNotifications :: Text -> Bool
isTabNotifications = (== "notifications")

isTabCourses :: Text -> Bool
isTabCourses = (== "courses")
