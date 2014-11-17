module Handler.CourseAssignmentManage where

import Import
import Control.Monad
import Yesod.Auth
import Handler.CourseSection
import AssignmentAction
import Data.Time
import qualified Data.Text as Text

dayLength :: NominalDiffTime
dayLength = 24 * 60 * 60

postCourseAssignmentManageR :: Text -> AssignmentId -> AssignmentAction -> Handler Html
postCourseAssignmentManageR cname aid action = do
  authId <- requireAuthId
  Entity courseId course <- runDB $ getBy404 $ UniqueCourse cname
  let isCourseOwner = authId == courseOwner course

  when (not isCourseOwner) $ do
    notFound

  now <- liftIO getCurrentTime
  assignment <- runDB $ get404 aid
  manageAssignment action now (Entity aid assignment)

  section <- runDB $ get404 $ assignmentSection assignment

  redirect $ CourseAssignmentR cname $ Text.splitOn "/" (sectionIdent section) ++ [assignmentIdent assignment]

manageAssignment :: AssignmentAction -> UTCTime -> Entity Assignment -> Handler Assignment
manageAssignment action now (Entity aid assignment) = do
  let newEndingAt = fmap (\n -> addUTCTime (realToFrac n) now) $ assignmentDuration assignment

  runDB $ update aid $
    case action of
      AssignmentLock      -> [AssignmentLocked    =. True]
      AssignmentUnlock    -> [AssignmentLocked    =. False]
      AssignmentStart     -> [AssignmentStartedAt =. Just now, AssignmentLocked =. False, AssignmentEndingAt =. newEndingAt]
      AssignmentStop      -> [AssignmentEndingAt  =. Just now]
      AssignmentExtraDay  -> [AssignmentEndingAt  =. addUTCTime    dayLength  <$> assignmentEndingAt assignment]
      AssignmentSkipDay   -> [AssignmentEndingAt  =. addUTCTime (- dayLength) <$> assignmentEndingAt assignment]
      AssignmentReset     -> [AssignmentLocked =. True, AssignmentStartedAt =. Nothing, AssignmentEndingAt =. Nothing]

  return assignment

