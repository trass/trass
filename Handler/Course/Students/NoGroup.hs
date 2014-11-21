module Handler.Course.Students.NoGroup where

import Import
import Handler.Course.Students

getCourseStudentsNoGroupR :: Text -> Handler Html
getCourseStudentsNoGroupR cname = displayCourseStudents cname DisplayNoGroup

postCourseStudentsNoGroupR :: Text -> Handler Html
postCourseStudentsNoGroupR cname = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  inviteStudent cid Nothing
  displayCourseStudents cname DisplayNoGroup
