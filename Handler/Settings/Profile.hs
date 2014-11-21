module Handler.Settings.Profile where

import Import
import Handler.Settings
import Yesod.Auth

getSettingsProfileR :: Handler Html
getSettingsProfileR = do
  authId <- requireAuthId
  Entity _ profile <- runDB $ getBy404 $ UniqueProfile authId
  let settingsTab = tabProfile profile

  defaultLayout $ do
    $(widgetFile "settings")
  where
    tabName = "profile"

postSettingsProfileR :: Handler Html
postSettingsProfileR = do
  authId <- requireAuthId
  name <- lookupPostParam "name"
  runDB $ updateWhere [ProfileUser ==. authId] [ProfileName =. name]
  redirect SettingsProfileR
