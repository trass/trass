module Handler.CourseSubmissionsByStatus where

import Import
import Yesod.Auth
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad
import Data.Time
import Data.Int
import Data.Maybe
import Text.Read (readMaybe)

import Handler.CourseAssignment (assignmentR)
import Handler.CourseSection (sectionR)

import SubmissionStatus
import UserRole
import Utils
import UtilsDB

getCourseSubmissionsByStatusR :: Text -> SubmissionStatus -> Handler Html
getCourseSubmissionsByStatusR cname status = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  userRole <- getUserRole cid authId
  when (not (isAssistant userRole || isTeacher userRole)) $ do
    notFound

  counts <- runDB $ getSubmissionCountsByStatus cid
  let
    countsMap = Map.fromList counts
    wSubmissionListItem s = do
      let isActive = s == status
      [whamlet|
        <a .list-group-item href="@{CourseSubmissionsByStatusR cname s}" :isActive:.active>
          <span .badge>&times; #{Map.findWithDefault 0 s countsMap}
          ^{wSubmissionStatus s}
      |]

  let totalSubmissions = Map.findWithDefault 0 status countsMap
  (pageNo, totalPages) <- pagerInfo totalSubmissions perPage

  submissions <- runDB $ getSubmissionsByStatus perPage pageNo cid status

  now <- liftIO getCurrentTime
  defaultLayout $ do
    $(widgetFile "teacher/submissions")

  where
    perPage = 20

