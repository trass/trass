module Handler.Course.Settings.InviteStaff where

import Import
import Handler.Course.Settings.Staff
import Yesod.Auth
import Control.Monad
import UserRole

postCourseSettingsInviteStaffR :: Text -> UserRole -> Handler Html
postCourseSettingsInviteStaffR cname role = do
  authId <- requireAuthId
  Entity cid course <- runDB $ getBy404 $ UniqueCourse cname
  let isCourseOwner = courseOwner course == authId
  when (not isCourseOwner) $ notFound
  inviteStaff cid role
  getCourseSettingsStaffR cname

inviteStaff :: CourseId -> UserRole -> Handler ()
inviteStaff cid role = do
  memail <- lookupPostParam "email"
  case memail of
    Nothing -> invalidArgs ["email"]
    Just email -> do
      muser <- runDB $ getBy $ UniqueUser email
      case muser of
        Nothing -> undefined
        Just (Entity uid _) -> runDB $ do
          mr <- getBy $ UniqueRole uid cid
          let r = Role uid cid role
          case mr of
            Nothing -> insert_ r
            Just (Entity rid _) -> replace rid r
