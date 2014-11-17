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

  pageNoStr <- lookupGetParam "page"

  let
    totalSubmissions = Map.findWithDefault 0 status countsMap
    totalPages = ceiling (fromIntegral totalSubmissions / fromIntegral perPage)

  pageNo <-
    case pageNoStr >>= readMaybe . Text.unpack of
      Nothing -> return 1
      Just n | 1 <= n && n <= totalPages -> return n
      _ -> notFound

  submissions <- runDB $ getSubmissionsByStatus perPage pageNo cid status

  now <- liftIO getCurrentTime
  defaultLayout $ do
    $(widgetFile "teacher/submissions")

  where
    perPage = 20
    pageR n = (CourseSubmissionsByStatusR cname status, [("page", Text.pack $ show $ n)])

