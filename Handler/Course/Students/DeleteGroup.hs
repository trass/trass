module Handler.Course.Students.DeleteGroup where

import Import
import Yesod.Auth
import UserRole
import Control.Monad

postCourseStudentsDeleteGroupR :: Text -> GroupId -> Handler Html
postCourseStudentsDeleteGroupR cname gid = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  role <- getUserRole cid authId
  when (not $ isTeacher role) $ notFound
  runDB $ do
    deleteWhere [GroupMemberGroup ==. gid]
    delete gid
  redirect $ CourseStudentsR cname
