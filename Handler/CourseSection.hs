module Handler.CourseSection where

import Import
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Time
import Data.Maybe
import Yesod.Auth
import Control.Monad (when)
import AssignmentAction
import UserRole
import Utils
import UtilsDB

mkSectionIdent :: [Text] -> Text
mkSectionIdent = Text.intercalate "/"

getSectionTitle :: CourseId -> [Text] -> Handler ([Text], Text)
getSectionTitle cid sids = do
  Entity _ section <- runDB $ getBy404 $ UniqueSection cid (mkSectionIdent sids)
  return (sids, sectionTitle section)

getCourseSectionR :: Text -> [Text] -> Handler Html
getCourseSectionR cname sids = do
  Entity cid course  <- runDB $ getBy404 $ UniqueCourse cname
  Entity sectionId section <- runDB $ getBy404 $ UniqueSection cid (mkSectionIdent sids)

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cid) mauthId

  sectionLocked <- runDB $ isSectionLocked sectionId
  when (not (isTeacher userRole) && sectionLocked) $ do
    notFound

  let sectionPaths = List.init . List.tail $ List.inits sids
  crumbs <- mapM (getSectionTitle cid) sectionPaths

  let
    isCourseOwner = mauthId == Just (courseOwner course)
    isCoursePreview   = True
    isCourseGraphs    = False
    isCourseSettings  = False
    headerTitle       = sectionTitle section
    headerSummary     = sectionSummary section
    courseHeader = $(widgetFile "course/header")

  subsections <- runDB $ selectList [SectionParent ==. Just sectionId] [Asc SectionTitle]
  lockedSubsections <- runDB $ mapM (isSectionLocked . entityKey) subsections
  assignments <- runDB $ selectList [AssignmentSection ==. sectionId] [Asc AssignmentTitle]

  setCourseIdent cname
  when (courseRootSection course == sectionId) $ do
    setCourseTitle (sectionTitle section)

  (saPoints, saDuration, saStartedAt, saEndingAt) <- runDB $ getSectionAssignmentInfo sectionId
  now <- liftIO getCurrentTime
  let
    inFuture dt = now < dt
    inPast = not . inFuture

  defaultLayout $ do
    $(widgetFile "course/section")

