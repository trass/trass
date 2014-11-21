module Handler.Settings.Account where

import Import
import Handler.Settings
import Yesod.Auth

getSettingsAccountR :: Handler Html
getSettingsAccountR = do
  _ <- requireAuthId
  defaultLayout $ do
    $(widgetFile "settings")
  where
    tabName = "account"
    settingsTab = tabAccount

