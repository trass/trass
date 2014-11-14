module Handler.CourseStudentsDeleteGroup where

import Import

postCourseStudentsDeleteGroupR :: Text -> GroupId -> Handler Html
postCourseStudentsDeleteGroupR cid gid = do
  runDB $ delete gid
  redirect $ CourseStudentsR cid
