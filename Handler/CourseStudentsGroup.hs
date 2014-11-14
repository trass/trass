module Handler.CourseStudentsGroup where

import Import
import Handler.CourseStudents

getCourseStudentsGroupR :: Text -> Text -> Handler Html
getCourseStudentsGroupR cid gname = displayCourseStudents cid (DisplayGroup gname)

postCourseStudentsGroupR :: Text -> Text -> Handler Html
postCourseStudentsGroupR = error "Not yet implemented: postCourseStudentsGroupR"
