module Handler.Settings where

import Import
import Yesod.Auth.Email (setpassR, forgotPasswordR)
import Data.Maybe

getSettingsR :: Handler Html
getSettingsR = redirect SettingsProfileR

tabProfile :: Profile -> Widget
tabProfile profile = $(widgetFile "settings/profile")

tabAccount :: Widget
tabAccount = $(widgetFile "settings/account")

tabStudentNotifications :: Widget
tabStudentNotifications = $(widgetFile "student/settings/notifications")

tabStudentCourses :: Widget
tabStudentCourses = $(widgetFile "student/settings/courses")

isTabProfile :: Text -> Bool
isTabProfile = (== "profile")

isTabAccount :: Text -> Bool
isTabAccount = (== "account")

isTabNotifications :: Text -> Bool
isTabNotifications = (== "notifications")

isTabCourses :: Text -> Bool
isTabCourses = (== "courses")
