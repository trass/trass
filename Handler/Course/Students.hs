module Handler.Course.Students where

import Import
import Yesod.Auth
import Data.Maybe
import UserRole
import Utils
import UtilsDB

data DisplayGroup
  = DisplayFirst
  | DisplayNoGroup
  | DisplayGroup Text

getCourseStudentsR :: Text -> Handler Html
getCourseStudentsR cname = displayCourseStudents cname DisplayFirst

postCourseStudentsR :: Text -> Handler Html
postCourseStudentsR cname = do
  mname <- lookupPostParam "groupName"
  case mname of
    Nothing -> invalidArgs ["groupName"]
    Just name -> do
      Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
      runDB $ insert_ $ Group cid name
  getCourseStudentsR cname

displayCourseStudents :: Text -> DisplayGroup -> Handler Html
displayCourseStudents cname dg = do
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
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

inviteStudent :: CourseId -> Maybe Text -> Handler ()
inviteStudent cid mgroup = do
  mname  <- lookupPostParam "studentName"
  memail <- lookupPostParam "studentEmail"
  case (,) <$> mname <*> memail of
    Nothing -> invalidArgs ["mname", "memail"]
    Just (name, email) -> do
      muser <- runDB $ getBy $ UniqueUser email
      case muser of
        Nothing -> undefined
        Just (Entity uid _) -> runDB $ do
          mrole <- getBy $ UniqueRole uid cid
          let role = Role uid cid RoleStudent
          case mrole of
            Nothing -> insert_ role
            Just (Entity rid _) -> replace rid role

          case mgroup of
            Nothing -> deleteWhere [GroupMemberStudent ==. uid, GroupMemberCourse ==. cid]
            Just gname -> do
              mg <- getBy $ UniqueGroup cid gname
              case mg of
                Nothing -> return ()
                Just (Entity gid _) -> do
                  mgm <- getBy $ UniqueGroupMember cid uid
                  let gm = GroupMember cid uid gid
                  case mgm of
                    Nothing -> insert_ gm
                    Just (Entity gmid _) -> replace gmid gm

