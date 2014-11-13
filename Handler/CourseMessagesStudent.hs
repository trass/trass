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
  when (authId /= studentId && isStudent role) $ notFound

  now <- liftIO getCurrentTime
  displayMessages courseI authId studentId now

postCourseMessagesStudentR :: Text -> UserId -> Handler Html
postCourseMessagesStudentR courseI studentId = do
  authId <- requireAuthId
  role <- getUserRole courseI authId
  when (authId /= studentId && isStudent role) $ notFound

  now <- liftIO getCurrentTime

  mmsg <- lookupPostParam "message"
  case mmsg of
    Just msg | not (Text.null msg) -> do
      runDB $ insert (Message courseI studentId authId now msg)
      return ()
    Nothing -> invalidArgs ["message"]
    _ -> return () -- silently ignore empty messages

  displayMessages courseI authId studentId now

displayMessages :: Text -> UserId -> UserId -> UTCTime -> Handler Html
displayMessages courseI authId studentId now = do
  messages    <- runDB $ selectList [MessageCourseIdent ==. courseI, MessageStudent ==. studentId] [Desc MessageDateTime]
  authors     <- runDB $ selectList [ProfileUser <-. map (messageAuthor . entityVal) messages] []
  readMsgIds  <- runDB $ selectList [ReadMessageReader ==. authId, ReadMessageMessage <-. map entityKey messages] []
  let
    readSet     = Set.fromList $ map (readMessageMessage . entityVal) readMsgIds
    authors'    = filter (isJust . profileName) $ map entityVal authors
    authorsMap  = Map.fromList $ map (profileUser &&& (fromJust . profileName)) authors'

  runDB $ forM_ messages $ \(Entity msgId _) -> do
    when (Set.notMember msgId readSet) $ do
      _ <- insert $ ReadMessage msgId authId
      return ()

  defaultLayout $ do
    $(widgetFile "student/messages")
