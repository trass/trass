module Handler.CourseSubmissionHistory where

import Import
import Yesod.Auth
import Handler.CourseSubmission
import Control.Monad
import Data.Time
import Data.Maybe
import qualified Data.Text as Text
import UserRole
import Utils

getCourseSubmissionHistoryR :: Text -> SubmissionId -> Handler Html
getCourseSubmissionHistoryR cname sid = do
  events <- runDB $ selectList [SubmissionEventSubmission ==. sid] [Asc SubmissionEventCreatedAt]
  now <- liftIO getCurrentTime
  courseSubmissionLayout cname sid "history" now $ do
    $(widgetFile "course/submission/history")

postCourseSubmissionHistoryR :: Text -> SubmissionId -> Handler Html
postCourseSubmissionHistoryR cname sid = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  submission <- runDB $ get404 sid

  userRole <- getUserRole cid authId
  when (isStudent userRole && submissionAuthor submission /= authId) $
    notFound

  mstatusStr <- lookupPostParam "status"
  let mstatus = mstatusStr >>= fromPathPiece

  when (isStudent userRole && isJust mstatusStr) $ do
    notFound

  comment <- lookupPostParam "comment"
  now <- liftIO getCurrentTime

  runDB $ do
    case comment of
      Just s | not (Text.null s) -> do
        insert_ $ SubmissionEvent sid Nothing Nothing comment now (Just authId)
      _ -> return ()
    case mstatus of
      Just status -> do
        update sid [SubmissionStatus =. status, SubmissionReviewer =. Just authId, SubmissionUpdatedAt =. now]
        insert_ $ SubmissionEvent sid (Just status) Nothing Nothing now (Just authId)
      Nothing -> return ()

  redirect $ CourseSubmissionHistoryR cname sid
