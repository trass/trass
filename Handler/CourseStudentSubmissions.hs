module Handler.CourseStudentSubmissions where

import Import
import Yesod.Auth
import Yesod.Core.Dispatch
import Control.Monad

import UserRole
import SubmissionStatus

import Data.Time
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Handler.CourseStudent
import Handler.CourseAssignment (assignmentR)
import Handler.CourseSection (sectionR)

import Utils
import UtilsDB

getCourseStudentSubmissionsR :: Text -> UserId -> Handler Html
getCourseStudentSubmissionsR cname uid = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cname
  Entity _ profile  <- runDB $ getBy404 $ UniqueProfile uid

  studentRole <- getUserRole cname uid
  when (not $ isStudent studentRole) $ notFound

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cname) mauthId

  let isOtherStudent = isStudent userRole && Just uid /= mauthId
  when isOtherStudent $ do
    notFound

  pageNoStr <- lookupGetParam "page"

  totalSubmissions <- runDB $ getSubmissionsCountByStudent courseId uid
  let totalPages = ceiling (fromIntegral totalSubmissions / fromIntegral perPage)

  pageNo <-
    case pageNoStr >>= readMaybe . Text.unpack of
      Nothing -> return 1
      Just n | 1 <= n && n <= totalPages -> return n
      _ -> notFound

  submissions <- runDB $ getSubmissionsByStudent perPage pageNo courseId uid

  now <- liftIO getCurrentTime
  let tab = $(widgetFile "course/student/submissions")

  defaultLayout $ do
    $(widgetFile "course/student")
  where
    nav :: Widget
    nav = $(widgetFile "course/student/nav")
    tabName = "submissions"
    perPage = 20
    pageR n = (CourseStudentSubmissionsR cname uid, [("page", Text.pack $ show $ n)])
