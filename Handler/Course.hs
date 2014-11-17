module Handler.Course where

import Import
import Handler.CourseSection

getCourseR :: Text -> Handler Html
getCourseR cname = do
  Entity _ course  <- runDB $ getBy404 $ UniqueCourse cname
  section <- runDB $ get404 (courseRootSection course)
  getCourseSectionR cname [sectionIdent section]
