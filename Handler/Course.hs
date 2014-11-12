module Handler.Course where

import Import
import qualified Data.Text as Text

getCourseR :: Text -> Handler Html
getCourseR courseI = do
  Entity _ course  <- runDB $ getBy404 $ UniqueCourse courseI
  let sectionId = courseRootSection course
  section <- runDB $ get404 sectionId
  subsections <- runDB $ selectList [SectionParent ==. Just sectionId] []

  setCourseIdent courseI
  setCourseTitle (sectionTitle section)

  defaultLayout $ do
    $(widgetFile "course-section")
