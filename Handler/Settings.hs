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

tabNotifications :: Widget
tabNotifications = $(widgetFile "student/settings/notifications")

tabCourses :: UserId -> Map Text UserRole -> [Entity Course] -> Widget
tabCourses authId roles courses = $(widgetFile "settings/courses")
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
