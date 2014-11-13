module Handler.SettingsNotifications where

import Import
import Handler.Settings
import Yesod.Auth

getSettingsNotificationsR :: Handler Html
getSettingsNotificationsR = do
  authId <- requireAuthId
  defaultLayout $ do
    $(widgetFile "settings")
  where
    tabName = "notifications"
    settingsTab = tabNotifications

