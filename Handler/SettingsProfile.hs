module Handler.SettingsProfile where

import Import
import Handler.Settings
import Yesod.Auth

getSettingsProfileR :: Handler Html
getSettingsProfileR = do
  authId <- requireAuthId
  Entity _ profile <- runDB $ getBy404 $ UniqueProfile authId
  defaultLayout $ do
    $(widgetFile "student/settings")
  where
    tabName = "profile"

postSettingsProfileR :: Handler Html
postSettingsProfileR = do
  authId <- requireAuthId
  name <- lookupPostParam "name"
  runDB $ updateWhere [ProfileUser ==. authId] [ProfileName =. name]
  redirect SettingsProfileR
