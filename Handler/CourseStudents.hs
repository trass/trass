module Handler.CourseStudents where

import Import
import Yesod.Auth
import Data.Maybe
import UserRole
import UtilsDB

data DisplayGroup
  = DisplayFirst
  | DisplayNoGroup
  | DisplayGroup Text

getCourseStudentsR :: Text -> Handler Html
getCourseStudentsR cid = displayCourseStudents cid DisplayFirst

postCourseStudentsR :: Text -> Handler Html
postCourseStudentsR cid = do
  mname <- lookupPostParam "groupName"
  case mname of
    Nothing -> invalidArgs ["groupName"]
    Just name -> do
      runDB $ insert_ $ Group cid name
  getCourseStudentsR cid

displayCourseStudents :: Text -> DisplayGroup -> Handler Html
displayCourseStudents cid dg = do
  groups <- runDB $ selectList [GroupCourse ==. cid] []
  chosenGroup <-
    case dg of
      DisplayGroup name -> Just <$> (runDB $ getBy404 $ UniqueGroup cid name)
      DisplayFirst      -> return $ listToMaybe groups
      DisplayNoGroup    -> return Nothing
  let
    chosenGid = entityKey <$> chosenGroup
    isChosen gid = Just gid == chosenGid
  students  <- runDB $ selectGroupMembers chosenGid
  mauthId   <- maybeAuthId
  userRole  <- maybe (return RoleStudent) (getUserRole cid) mauthId
  defaultLayout $ do
    $(widgetFile "course/students")

inviteStudent :: Text -> Maybe Text -> Handler Html
inviteStudent cid mgroup = do
  mname  <- lookupPostParam "studentName"
  memail <- lookupPostParam "studentEmail"
  case (,) <$> mname <*> memail of
    Nothing -> invalidArgs ["mname", "memail"]
    Just (name, email) -> do
      muid <- runDB $ getBy $ UniqueUser email
      case muid of
        Nothing -> do
          undefined
        _ -> undefined
