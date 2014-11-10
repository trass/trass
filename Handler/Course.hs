module Handler.Course where

import Import

getCourseR :: Text -> Handler Html
getCourseR courseI = do
  Entity _ course  <- runDB $ getBy404 $ UniqueCourse courseI
  let sectionId = courseRootSection course
  section <- runDB $ get404 sectionId
  subsections <- runDB $ selectList [SectionParent ==. Just sectionId] []
  defaultLayout $ do
    $(widgetFile "course-section")
