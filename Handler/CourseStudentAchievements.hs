module Handler.CourseStudentAchievements where

import Import
import Yesod.Auth
import Yesod.Core.Dispatch
import Control.Monad

import UserRole
import SubmissionStatus
import Achievement

import Data.Time
import qualified Data.Text as Text
import Text.Read (readMaybe)

import Handler.CourseStudent
import Handler.CourseAssignment (assignmentR)
import Handler.CourseSection (sectionR)

import Utils
import UtilsDB

getCourseStudentAchievementsR :: Text -> UserId -> Handler Html
getCourseStudentAchievementsR cname uid = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cname
  Entity _ profile  <- runDB $ getBy404 $ UniqueProfile uid

  studentRole <- getUserRole cname uid
  when (not $ isStudent studentRole) $ notFound

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cname) mauthId

  let isOtherStudent = isStudent userRole && Just uid /= mauthId
  when isOtherStudent $ do
    notFound

  coursePoints <- runDB $ getStudentCoursePointsSum courseId uid
  achievementTotals <- runDB $ getStudentAchievementsTotal courseId uid

  goldAchievements    <- runDB $ getStudentAchievements courseId uid AchievementGold
  silverAchievements  <- runDB $ getStudentAchievements courseId uid AchievementSilver
  bronzeAchievements  <- runDB $ getStudentAchievements courseId uid AchievementBronze
  secretAchievements  <- runDB $ getStudentAchievements courseId uid AchievementSecret
  let
    achievementGroups = [goldAchievements, silverAchievements, bronzeAchievements, secretAchievements]
    tab = $(widgetFile "course/student/achievements")

  defaultLayout $ do
    $(widgetFile "course/student")
  where
    nav :: Widget
    nav = $(widgetFile "course/student/nav")
    tabName = "achievements"

postCourseStudentAchievementsR :: Text -> UserId -> Handler Html
postCourseStudentAchievementsR = error "Not yet implemented: postCourseStudentAchievementsR"
