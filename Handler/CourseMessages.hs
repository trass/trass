module Handler.CourseMessages where

import Import
import Control.Arrow ((&&&))
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Time
import Yesod.Auth
import Utils

getCourseMessagesR :: Text -> Handler Html
getCourseMessagesR courseI = do
  authId <- requireAuthId
  now <- liftIO getCurrentTime
  displayMessages courseI authId now

postCourseMessagesR :: Text -> Handler Html
postCourseMessagesR courseI = do
  authId <- requireAuthId
  now <- liftIO getCurrentTime

  mmsg <- lookupPostParam "message"
  case mmsg of
    Just msg | not (Text.null msg) -> do
      runDB $ insert (Message courseI authId authId now msg)
      return ()
    Nothing -> invalidArgs ["message"]
    _ -> return () -- silently ignore empty messages

  displayMessages courseI authId now

displayMessages :: Text -> UserId -> UTCTime -> Handler Html
displayMessages courseI authId now = do
  messages    <- runDB $ selectList [MessageCourseIdent ==. courseI, MessageStudent ==. authId] [Desc MessageDateTime]
  authors     <- runDB $ selectList [ProfileUser <-. map (messageAuthor . entityVal) messages] []
  let
    authors'    = filter (isJust . profileName) $ map entityVal authors
    authorsMap  = Map.fromList $ map (profileUser &&& (fromJust . profileName)) authors'

  defaultLayout $ do
    $(widgetFile "student/messages")
