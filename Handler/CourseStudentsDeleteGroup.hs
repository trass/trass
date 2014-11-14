module Handler.CourseStudentsDeleteGroup where

import Import

postCourseStudentsDeleteGroupR :: Text -> GroupId -> Handler Html
postCourseStudentsDeleteGroupR cid gid = do
  runDB $ do
    deleteWhere [GroupMemberGroup ==. gid]
    delete gid
  redirect $ CourseStudentsR cid
