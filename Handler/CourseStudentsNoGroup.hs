module Handler.CourseStudentsNoGroup where

import Import
import Handler.CourseStudents

getCourseStudentsNoGroupR :: Text -> Handler Html
getCourseStudentsNoGroupR cid = displayCourseStudents cid DisplayNoGroup

postCourseStudentsNoGroupR :: Text -> Handler Html
postCourseStudentsNoGroupR cid = do
  inviteStudent cid Nothing
  displayCourseStudents cid DisplayNoGroup