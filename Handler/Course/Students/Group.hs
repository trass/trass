module Handler.Course.Students.Group where

import Import
import Handler.Course.Students

getCourseStudentsGroupR :: Text -> Text -> Handler Html
getCourseStudentsGroupR cname gname = displayCourseStudents cname (DisplayGroup gname)

postCourseStudentsGroupR :: Text -> Text -> Handler Html
postCourseStudentsGroupR cname gname = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  inviteStudent cid (Just gname)
  displayCourseStudents cname (DisplayGroup gname)
