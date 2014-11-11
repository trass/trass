module Handler.CourseMessages where

import Import
import qualified Data.Text as Text
import Data.Time
import Yesod.Auth
import Language
import Utils

getCourseMessagesR :: Text -> Handler Html
getCourseMessagesR courseI = do
  authId <- requireAuthId
  messages <- runDB $ selectList [MessageCourseIdent ==. courseI, MessageStudent ==. authId] [Desc MessageDateTime]
  now <- liftIO getCurrentTime
  defaultLayout $ do
    $(widgetFile "student/messages")

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

  messages <- runDB $ selectList [MessageCourseIdent ==. courseI, MessageStudent ==. authId] [Desc MessageDateTime]
  defaultLayout $ do
    $(widgetFile "student/messages")
