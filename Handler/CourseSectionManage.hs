module Handler.CourseSectionManage where

import Import
import Control.Monad
import Yesod.Auth
import Handler.CourseSection
import AssignmentAction
import Data.Time
import qualified Data.Text as Text

dayLength :: NominalDiffTime
dayLength = 24 * 60 * 60

postCourseSectionManageR :: Text -> SectionId -> AssignmentAction -> Handler Html
postCourseSectionManageR cid sid action = do
  Entity courseId course <- runDB $ getBy404 $ UniqueCourse cid
  authId <- requireAuthId
  let isCourseOwner = authId == courseOwner course

  when (not isCourseOwner) $ do
    notFound

  now <- liftIO getCurrentTime
  section <- runDB $ get404 sid

  runDB $ updateWhere [AssignmentSection ==. sid] $
    case action of
      AssignmentLock      -> [AssignmentLocked    =. True]
      AssignmentUnlock    -> [AssignmentLocked    =. False]
      AssignmentStart     -> [AssignmentStartedAt =. Just now, AssignmentLocked =. False]
      AssignmentStop      -> [AssignmentEndingAt  =. Just now]
      AssignmentReset     -> [AssignmentLocked =. True, AssignmentStartedAt =. Nothing, AssignmentEndingAt =. Nothing]
      _ -> []

  redirect $ CourseSectionR cid $ Text.splitOn "/" (sectionIdent section)

