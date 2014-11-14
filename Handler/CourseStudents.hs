module Handler.CourseStudents where

import Import
import Yesod.Auth
import Data.Maybe
import UserRole
import UtilsDB

getCourseStudentsR :: Text -> Handler Html
getCourseStudentsR cid = do
  mauthId <- maybeAuthId
  userRole <- maybe (return RoleStudent) (getUserRole cid) mauthId
  groups <- runDB $ selectList [GroupCourse ==. cid] []
  let chosenGroup = listToMaybe groups
  students <- runDB $ selectGroupMembers (entityKey <$> chosenGroup)
  defaultLayout $ do
    $(widgetFile "course/students")

postCourseStudentsR :: Text -> Handler Html
postCourseStudentsR cid = do
  mname <- lookupPostParam "groupName"
  case mname of
    Nothing -> invalidArgs ["groupName"]
    Just name -> do
      runDB $ insert_ $ Group cid name
  getCourseStudentsR cid
