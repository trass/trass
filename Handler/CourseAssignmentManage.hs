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
postCourseAssignmentManageR cid aid action = do
  Entity courseId course <- runDB $ getBy404 $ UniqueCourse cid
  authId <- requireAuthId
  let isCourseOwner = authId == courseOwner course

  when (not isCourseOwner) $ do
    notFound

  now <- liftIO getCurrentTime
  assignment <- runDB $ get404 aid

  runDB $ update aid $
    case action of
      AssignmentLock      -> [AssignmentLocked    =. True]
      AssignmentUnlock    -> [AssignmentLocked    =. False]
      AssignmentStart     -> [AssignmentStartedAt =. Just now, AssignmentLocked =. False]
      AssignmentStop      -> [AssignmentEndingAt  =. Just now]
      AssignmentExtraDay  -> [AssignmentEndingAt  =. addUTCTime    dayLength  <$> assignmentEndingAt assignment]
      AssignmentSkipDay   -> [AssignmentEndingAt  =. addUTCTime (- dayLength) <$> assignmentEndingAt assignment]
      AssignmentReset     -> [AssignmentLocked =. True, AssignmentStartedAt =. Nothing, AssignmentEndingAt =. Nothing]

  section    <- runDB $ get404 $ assignmentSection assignment

  redirect $ CourseAssignmentR cid $ Text.splitOn "/" (sectionIdent section) ++ [assignmentIdent assignment]

