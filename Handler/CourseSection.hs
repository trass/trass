module Handler.CourseSection where

import Import
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Maybe
import Yesod.Auth
import Control.Monad (when)
import UserRole

mkSectionIdent :: [Text] -> Text
mkSectionIdent = Text.intercalate "/"

getSectionTitle :: CourseId -> [Text] -> Handler ([Text], Text)
getSectionTitle courseId sids = do
  Entity _ section <- runDB $ getBy404 $ UniqueSection courseId (mkSectionIdent sids)
  return (sids, sectionTitle section)

getCourseSectionR :: Text -> [Text] -> Handler Html
getCourseSectionR cid sids = do
  Entity courseId course  <- runDB $ getBy404 $ UniqueCourse cid
  Entity sectionId section <- runDB $ getBy404 $ UniqueSection courseId (mkSectionIdent sids)

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cid) mauthId

  when (sectionLocked section && not (isTeacher userRole)) $ do
    notFound

  let sectionPaths = List.init . List.tail $ List.inits sids
  crumbs <- mapM (getSectionTitle courseId) sectionPaths

  let
    isCourseOwner = mauthId == Just (courseOwner course)
    isCoursePreview   = True
    isCourseGraphs    = False
    isCourseSettings  = False
    headerTitle       = sectionTitle section
    headerSummary     = sectionSummary section
    courseHeader = $(widgetFile "course/header")

  subsections <- runDB $ selectList [SectionParent ==. Just sectionId] [Asc SectionTitle]
  assignments <- runDB $ selectList [AssignmentSection ==. sectionId] [Asc AssignmentTitle]

  setCourseIdent cid
  when (courseRootSection course == sectionId) $ do
    setCourseTitle (sectionTitle section)

  defaultLayout $ do
    $(widgetFile "course/section")
