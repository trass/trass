module Handler.CourseMessagesStudent where

import Import
import Control.Arrow ((&&&))
import Control.Monad (when, forM_)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time
import Yesod.Auth
import Utils
import UserRole

getCourseMessagesStudentR :: Text -> UserId -> Handler Html
getCourseMessagesStudentR courseI studentId = do
  authId <- requireAuthId
  role <- getUserRole courseI authId
  when (authId /= studentId && not (isTeacher role)) $ notFound

  now <- liftIO getCurrentTime
  displayMessages courseI studentId authId role now

postCourseMessagesStudentR :: Text -> UserId -> Handler Html
postCourseMessagesStudentR courseI studentId = do
  authId <- requireAuthId
  role <- getUserRole courseI authId
  when (authId /= studentId && not (isTeacher role)) $ notFound

  now <- liftIO getCurrentTime

  mmsg <- lookupPostParam "message"
  case mmsg of
    Just msg | not (Text.null msg) -> do
      runDB $ do
        mid <- insert (Message courseI studentId authId now msg)
        lm <- getBy $ UniqueLastMessage courseI studentId
        case lm of
          Nothing -> insert_ (LastMessage courseI studentId mid)
          Just (Entity lmid _) -> replace lmid (LastMessage courseI studentId mid)
        return ()
    Nothing -> invalidArgs ["message"]
    _ -> return () -- silently ignore empty messages

  displayMessages courseI studentId authId role now

displayMessages :: Text -> UserId -> UserId -> UserRole -> UTCTime -> Handler Html
displayMessages courseI studentId authId userRole now = do
  messages    <- runDB $ selectList [MessageCourseIdent ==. courseI, MessageStudent ==. studentId] [Desc MessageDateTime]
  let authorIds = map (messageAuthor . entityVal) messages
  authors     <- runDB $ selectList [ProfileUser <-. authorIds] []
  roles       <- runDB $ selectList [RoleUser <-. authorIds] []
  readMsgIds  <- runDB $ selectList [ReadMessageReader ==. authId, ReadMessageMessage <-. map entityKey messages] []
  let
    readSet     = Set.fromList $ map (readMessageMessage . entityVal) readMsgIds
    authors'    = filter (isJust . profileName) $ map entityVal authors
    authorsMap  = Map.fromList $ map (profileUser &&& (fromJust . profileName)) authors'
    rolesMap    = Map.fromList $ map ((roleUser &&& roleRole) . entityVal) roles
    msgs        = map (\msg -> (msg, rolesMap Map.! messageAuthor (entityVal msg))) messages

  runDB $ forM_ messages $ \(Entity msgId _) -> do
    when (Set.notMember msgId readSet) $ do
      _ <- insert $ ReadMessage msgId authId
      return ()

  defaultLayout $ do
    $(widgetFile "student/messages")

