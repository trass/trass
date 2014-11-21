module Handler.Settings.Notifications where

import Import
import Handler.Settings
import Yesod.Auth
import UserRole

getSettingsNotificationsR :: Handler Html
getSettingsNotificationsR = do
  authId <- requireAuthId
  userRole <- mUserRole authId
  let settingsTab = tabNotifications userRole
  defaultLayout $ do
    $(widgetFile "settings")
  where
    tabName = "notifications"

mUserRole :: UserId -> Handler UserRole
mUserRole uid = do
  mcname <- getCourseIdent
  case mcname of
    Nothing -> return RoleStudent
    Just cname -> do
      Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
      getUserRole cid uid
