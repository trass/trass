module Handler.CourseSection where

import Import
import qualified Data.Text as Text

mkSectionIdent :: [Text] -> Text
mkSectionIdent = Text.intercalate "/"

getCourseSectionR :: Text -> [Text] -> Handler Html
getCourseSectionR courseI sids = do
  Entity sectionId section <- runDB $ getBy404 $ UniqueSection courseI (mkSectionIdent sids)
  subsections <- runDB $ selectList [SectionParent ==. Just sectionId] []
  defaultLayout $ do
    $(widgetFile "course-section")
