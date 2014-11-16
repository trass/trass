module Handler.CourseStudent where

import Import

getCourseStudentR :: Text -> UserId -> Handler Html
getCourseStudentR cid uid = redirect $ CourseStudentCoursePointsR cid uid

isTabAchievements :: Text -> Bool
isTabAchievements = (== "achievements")

isTabCoursePoints :: Text -> Bool
isTabCoursePoints = (== "points")

isTabRating :: Text -> Bool
isTabRating = (== "rating")

isTabSubmissions :: Text -> Bool
isTabSubmissions = (== "submissions")

isTabAssignments :: Text -> Bool
isTabAssignments = (== "assignments")

isTabConversation :: Text -> Bool
isTabConversation = (== "conversation")
