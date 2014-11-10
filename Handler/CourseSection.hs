module Handler.CourseSection where

import Import

getCourseSectionR :: Text -> Text -> Handler Html
getCourseSectionR courseI sectionI = do
  Entity sectionId section <- runDB $ getBy404 $ UniqueSection courseI sectionI
  subsections <- runDB $ selectList [SectionParent ==. Just sectionId] []
  defaultLayout $ do
    $(widgetFile "course-section")
