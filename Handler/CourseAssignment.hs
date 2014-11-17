module Handler.CourseAssignment where

import Import
import Yesod.Auth
import Control.Monad
import qualified Data.Text as Text
import qualified Data.List as List
import Handler.CourseSection
import Data.Maybe
import Data.Time
import AssignmentAction
import UserRole
import Utils

getCourseAssignmentR :: Text -> [Text] -> Handler Html
getCourseAssignmentR cname path@(_:_:_) = do
  let
    sids = List.init path
    aid  = List.head $ reverse path

  Entity cid course <- runDB $ getBy404 $ UniqueCourse cname
  Entity sectionId section <- runDB $ getBy404 $ UniqueSection cid (mkSectionIdent sids)

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cid) mauthId

  Entity assignmentId assignment <- runDB $ getBy404 $ UniqueAssignment sectionId aid

  when (assignmentLocked assignment && not (isTeacher userRole)) $ do
    notFound

  let sectionPaths = List.tail $ List.inits sids
  crumbs <- mapM (getSectionTitle cid) sectionPaths

  let
    isCourseOwner = mauthId == Just (courseOwner course)
    isCoursePreview   = True
    isCourseGraphs    = False
    isCourseSettings  = False
    headerTitle       = assignmentTitle assignment
    headerSummary     = assignmentSummary assignment
    courseHeader = $(widgetFile "course/header")

  setCourseIdent cname

  now <- liftIO getCurrentTime
  let
    inFuture dt = now < dt
    inPast = not . inFuture

  defaultLayout $ do
    addStylesheet $ StaticR codemirror_4_7_lib_codemirror_css
    addScript     $ StaticR codemirror_4_7_lib_codemirror_js
    $(widgetFile "course/assignment")
getCourseAssignmentR _ _ = notFound

postCourseAssignmentR :: Text -> [Text] -> Handler Html
postCourseAssignmentR = error "Not yet implemented: postCourseAssignmentR"

-- assignmentR :: Text -> Assignment -> Section -> Route App
assignmentR cname a s = CourseAssignmentR cname (Text.splitOn "/" (sectionIdent s) ++ [assignmentIdent a])

wAssignmentLink :: Text -> Assignment -> Section -> Widget
wAssignmentLink cname assignment section = do
  [whamlet|
    <a href="@{assignmentR cname assignment section}">#{assignmentTitle assignment}
  |]

