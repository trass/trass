module Handler.CourseSubmissionsByStatus where

import Import
import SubmissionStatus
import qualified Data.Map as Map
import Data.Time

import Utils
import UtilsDB

getCourseSubmissionsByStatusR :: Text -> SubmissionStatus -> Handler Html
getCourseSubmissionsByStatusR cid status = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cid
  submissions <- runDB $ selectList [SubmissionStatus ==. status] [Desc SubmissionUpdatedAt]

  counts <- runDB $ getSubmissionCountsByStatus courseId
  let
    countsMap = Map.fromList counts
    wSubmissionListItem s = do
      let isActive = s == status
      [whamlet|
        <a .list-group-item href="@{CourseSubmissionsByStatusR cid s}" :isActive:.active>
          <span .badge>&times; #{Map.findWithDefault 0 s countsMap}
          ^{wSubmissionStatus s}
      |]

  now <- liftIO getCurrentTime
  defaultLayout $ do
    $(widgetFile "teacher/submissions")
