module Handler.CourseAchievements where

import Import
import Yesod.Auth
import UserRole
import Achievement
import Utils

getCourseAchievementsR :: Text -> Handler Html
getCourseAchievementsR cname = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname

  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cname) mauthId

  let showSecretAchievements = isTeacher userRole

  achievements <- runDB $ selectList [AchievementCourse ==. cid] []

  defaultLayout $ do
    $(widgetFile "course/achievements")
