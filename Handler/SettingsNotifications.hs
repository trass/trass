module Handler.SettingsNotifications where

import Import
import Handler.Settings
import Yesod.Auth
import UserRole

getSettingsNotificationsR :: Handler Html
getSettingsNotificationsR = do
  authId <- requireAuthId
  mcid <- getCourseIdent
  userRole <-
    case mcid of
      Nothing -> return RoleStudent
      Just cid -> getUserRole cid authId
  let settingsTab = tabNotifications userRole
  defaultLayout $ do
    $(widgetFile "settings")
  where
    tabName = "notifications"

