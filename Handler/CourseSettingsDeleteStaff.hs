module Handler.CourseSettingsDeleteStaff where

import Import
import Yesod.Auth
import Control.Monad

postCourseSettingsDeleteStaffR :: Text -> UserId -> Handler Html
postCourseSettingsDeleteStaffR cname uid = do
  authId <- requireAuthId
  Entity cid course <- runDB $ getBy404 $ UniqueCourse cname
  let isCourseOwner = courseOwner course == authId
  when (not isCourseOwner) $ do
    notFound
  runDB $ deleteWhere [RoleCourse ==. cid, RoleUser ==. uid]
  redirect $ CourseSettingsStaffR cname
