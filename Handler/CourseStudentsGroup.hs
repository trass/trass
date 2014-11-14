module Handler.CourseStudentsGroup where

import Import
import Handler.CourseStudents

getCourseStudentsGroupR :: Text -> Text -> Handler Html
getCourseStudentsGroupR cid gname = displayCourseStudents cid (DisplayGroup gname)

postCourseStudentsGroupR :: Text -> Text -> Handler Html
postCourseStudentsGroupR cid gname = do
  inviteStudent cid (Just gname)
  displayCourseStudents cid (DisplayGroup gname)
