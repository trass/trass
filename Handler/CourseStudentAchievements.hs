module Handler.CourseStudentAchievements where

import Import
import Achievement
import Handler.CourseStudent
import Utils
import UtilsDB

getCourseStudentAchievementsR :: Text -> UserId -> Handler Html
getCourseStudentAchievementsR cname uid = do
  Entity courseId _ <- runDB $ getBy404 $ UniqueCourse cname

  goldAchievements    <- runDB $ getStudentAchievements courseId uid AchievementGold
  silverAchievements  <- runDB $ getStudentAchievements courseId uid AchievementSilver
  bronzeAchievements  <- runDB $ getStudentAchievements courseId uid AchievementBronze
  secretAchievements  <- runDB $ getStudentAchievements courseId uid AchievementSecret
  let
    achievementGroups = [goldAchievements, silverAchievements, bronzeAchievements, secretAchievements]

  courseStudentLayout cname uid "achievements" $ do
    $(widgetFile "course/student/achievements")

postCourseStudentAchievementsR :: Text -> UserId -> Handler Html
postCourseStudentAchievementsR = error "Not yet implemented: postCourseStudentAchievementsR"
