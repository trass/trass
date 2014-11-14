module Handler.CourseStudents where

import Import
import Data.Maybe
import UtilsDB

getCourseStudentsR :: Text -> Handler Html
getCourseStudentsR cid = do
  groups <- runDB $ selectList [GroupCourse ==. cid] []
  let chosenGroup = listToMaybe groups
  students <- runDB $ selectGroupMembers (entityKey <$> chosenGroup)
  defaultLayout $ do
    $(widgetFile "course/students")

postCourseStudentsR :: Text -> Handler Html
postCourseStudentsR = error "Not yet implemented: postCourseStudentsR"
