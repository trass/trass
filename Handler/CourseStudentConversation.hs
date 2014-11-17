module Handler.CourseStudentConversation where

import Import
import Yesod.Auth
import Handler.CourseStudent
import Control.Arrow
import Control.Monad
import Data.Time
import Data.Function
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map as Map
import UserRole
import Utils
import UtilsDB

getCourseStudentConversationR :: Text -> UserId -> Handler Html
getCourseStudentConversationR cname uid = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  role <- getUserRole cid authId
  when (authId /= uid && not (isTeacher role)) $ notFound

  msgs <- runDB $ getConversationMessages cid uid authId
  let
    msgs'       = map (\(Entity _ m, r) -> (m, r)) msgs
    msgGroups   = List.groupBy ((==) `on` (messageAuthor . fst)) msgs'
    authorIds   = map (messageAuthor . fst) msgs'
  authors <- runDB $ selectList [ProfileUser <-. authorIds] []
  roles   <- runDB $ selectList [RoleCourse ==. cid, RoleUser <-. authorIds] []
  let
    authorsMap = Map.fromList $ map (profileUser &&& id)    $ map entityVal authors
    rolesMap   = Map.fromList $ map (roleUser &&& roleRole) $ map entityVal roles
    messageGroups = map (\ms@((m,_):_) -> (authorsMap Map.! messageAuthor m, rolesMap Map.! messageAuthor m, ms)) msgGroups

  runDB $ forM_ msgs $ \(Entity msgId _, isUnread) -> do
    when isUnread $ do
      insert_ $ ReadMessage msgId authId

  now <- liftIO getCurrentTime
  let groupByTime = List.groupBy ((==) `on` (flip ago now . messageDateTime . fst))
  courseStudentLayout cname uid "conversation" $ do
    $(widgetFile "course/student/conversation")

postCourseStudentConversationR :: Text -> UserId -> Handler Html
postCourseStudentConversationR cname uid = do
  authId <- requireAuthId
  Entity cid _ <- runDB $ getBy404 $ UniqueCourse cname
  role <- getUserRole cid authId
  when (authId /= uid && not (isTeacher role)) $ notFound

  now <- liftIO getCurrentTime

  mmsg <- lookupPostParam "message"
  case mmsg of
    Just msg | not (Text.null msg) -> do
      runDB $ do
        mid <- insert (Message cid uid authId now msg)
        lm <- getBy $ UniqueLastMessage cid uid
        case lm of
          Nothing -> insert_ (LastMessage cid uid mid)
          Just (Entity lmid _) -> replace lmid (LastMessage cid uid mid)
        return ()
    Nothing -> invalidArgs ["message"]
    _ -> return () -- silently ignore empty messages

  redirect $ CourseStudentConversationR cname uid

