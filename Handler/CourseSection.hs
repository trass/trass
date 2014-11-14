module Handler.CourseSection where

import Import
import qualified Data.Text as Text
import Yesod.Auth
import UserRole

mkSectionIdent :: [Text] -> Text
mkSectionIdent = Text.intercalate "/"

getCourseSectionR :: Text -> [Text] -> Handler Html
getCourseSectionR cid sids = do
  Entity _ course  <- runDB $ getBy404 $ UniqueCourse cid
  Entity sectionId section <- runDB $ getBy404 $ UniqueSection cid (mkSectionIdent sids)
  subsections <- runDB $ selectList [SectionParent ==. Just sectionId] []

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cid) mauthId
  let
    isCourseOwner = mauthId == Just (courseOwner course)
    isCoursePreview   = True
    isCourseGraphs    = False
    isCourseSettings  = False
    courseHeader = $(widgetFile "course/header")

  defaultLayout $ do
    $(widgetFile "course/section")
