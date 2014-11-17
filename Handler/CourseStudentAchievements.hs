module Handler.CourseStudentAchievements where

import Import
import Achievement
import Handler.CourseStudent
import Utils
import UtilsDB

getCourseStudentAchievementsR :: Text -> UserId -> Handler Html
getCourseStudentAchievementsR cname uid = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname

  goldAchievements    <- runDB $ getStudentAchievements cid uid AchievementGold
  silverAchievements  <- runDB $ getStudentAchievements cid uid AchievementSilver
  bronzeAchievements  <- runDB $ getStudentAchievements cid uid AchievementBronze
  secretAchievements  <- runDB $ getStudentAchievements cid uid AchievementSecret
  let
    achievementGroups = [goldAchievements, silverAchievements, bronzeAchievements, secretAchievements]

  courseStudentLayout cname uid "achievements" $ do
    $(widgetFile "course/student/achievements")

postCourseStudentAchievementsR :: Text -> UserId -> Handler Html
postCourseStudentAchievementsR = error "Not yet implemented: postCourseStudentAchievementsR"
