module Handler.Course where

import Import
import Handler.CourseSection

getCourseR :: Text -> Handler Html
getCourseR cid = do
  Entity _ course  <- runDB $ getBy404 $ UniqueCourse cid
  section <- runDB $ get404 (courseRootSection course)
  getCourseSectionR cid [sectionIdent section]
