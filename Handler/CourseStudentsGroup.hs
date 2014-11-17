module Handler.CourseStudentsGroup where

import Import
import Handler.CourseStudents

getCourseStudentsGroupR :: Text -> Text -> Handler Html
getCourseStudentsGroupR cname gname = displayCourseStudents cname (DisplayGroup gname)

postCourseStudentsGroupR :: Text -> Text -> Handler Html
postCourseStudentsGroupR cname gname = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  inviteStudent cid (Just gname)
  displayCourseStudents cname (DisplayGroup gname)
