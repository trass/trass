module Handler.CourseSectionManage where

import Import
import Control.Monad
import Yesod.Auth
import Handler.CourseSection
import Handler.CourseAssignmentManage
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
  assignments <- runDB $ selectList [AssignmentSection ==. sid] []
  mapM_ (manageAssignment action now) assignments

  redirect $ CourseSectionR cid $ Text.splitOn "/" (sectionIdent section)

