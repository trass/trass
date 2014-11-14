module Handler.CourseStudentsDeleteGroup where

import Import
import Yesod.Auth
import UserRole
import Control.Monad

postCourseStudentsDeleteGroupR :: Text -> GroupId -> Handler Html
postCourseStudentsDeleteGroupR cid gid = do
  authId <- requireAuthId
  role <- getUserRole cid authId
  when (not $ isTeacher role) $ notFound
  runDB $ do
    deleteWhere [GroupMemberGroup ==. gid]
    delete gid
  redirect $ CourseStudentsR cid
