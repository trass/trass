module Handler.SettingsAccount where

import Import
import Handler.Settings
import Yesod.Auth

getSettingsAccountR :: Handler Html
getSettingsAccountR = do
  authId <- requireAuthId
  defaultLayout $ do
    $(widgetFile "student/settings")
  where
    tabName = "account"
    settingsTab = tabAccount

postSettingsAccountR :: Handler Html
postSettingsAccountR = error "Not yet implemented: postSettingsAccountR"
