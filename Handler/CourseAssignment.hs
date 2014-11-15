module Handler.CourseAssignment where

import Import
import Yesod.Auth
import Control.Monad
import qualified Data.List as List
import Handler.CourseSection
import Data.Maybe
import Data.Time
import AssignmentAction
import UserRole

getCourseAssignmentR :: Text -> [Text] -> Handler Html
getCourseAssignmentR cid path@(_:_:_) = do
  let
    sids = List.init path
    aid  = List.head $ reverse path

  Entity courseId  course  <- runDB $ getBy404 $ UniqueCourse cid
  Entity sectionId section <- runDB $ getBy404 $ UniqueSection courseId (mkSectionIdent sids)

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cid) mauthId

  when (sectionLocked section && not (isTeacher userRole)) $ do
    notFound

  Entity assignmentId assignment <- runDB $ getBy404 $ UniqueAssignment sectionId aid

  let sectionPaths = List.tail $ List.inits sids
  crumbs <- mapM (getSectionTitle courseId) sectionPaths

  let
    isCourseOwner = mauthId == Just (courseOwner course)
    isCoursePreview   = True
    isCourseGraphs    = False
    isCourseSettings  = False
    headerTitle       = assignmentTitle assignment
    headerSummary     = assignmentSummary assignment
    courseHeader = $(widgetFile "course/header")

  setCourseIdent cid

  now <- liftIO getCurrentTime
  let
    inFuture dt = now < dt
    inPast = not . inFuture
  defaultLayout $ do
    $(widgetFile "course/assignment")
getCourseAssignmentR _ _ = notFound

postCourseAssignmentR :: Text -> [Text] -> Handler Html
postCourseAssignmentR = error "Not yet implemented: postCourseAssignmentR"
