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
postCourseSectionManageR cname sid action = do
  authId <- requireAuthId
  Entity cid course <- runDB $ getBy404 $ UniqueCourse cname
  let isCourseOwner = authId == courseOwner course

  when (not isCourseOwner) $ do
    notFound

  now <- liftIO getCurrentTime
  section <- runDB $ get404 sid
  assignments <- runDB $ selectList [AssignmentSection ==. sid] []
  mapM_ (manageAssignment action now) assignments

  redirect $ CourseSectionR cname $ Text.splitOn "/" (sectionIdent section)

