module Handler.CourseSettingsDeleteStaff where

import Import
import Yesod.Auth
import Control.Monad

postCourseSettingsDeleteStaffR :: Text -> UserId -> Handler Html
postCourseSettingsDeleteStaffR cid uid = do
  authId <- requireAuthId
  Entity _ course <- runDB $ getBy404 $ UniqueCourse cid
  let isCourseOwner = courseOwner course == authId
  when (not isCourseOwner) $ do
    notFound
  runDB $ deleteWhere [RoleCourse ==. cid, RoleUser ==. uid]
  redirect $ CourseSettingsStaffR cid
