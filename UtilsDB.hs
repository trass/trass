{-# LANGUAGE ScopedTypeVariables #-}
module UtilsDB where

import Prelude
import Model
import UserRole
import Data.Text (Text)
import Database.Esqueleto
import Control.Monad
import Control.Monad.IO.Class (MonadIO)

countUnreadMessages :: (Functor m, MonadIO m) => Text -> UserId -> UserRole -> SqlPersistT m Int
countUnreadMessages cid uid role = liftM (unValue . head) $ do
  select $
    from $ \msg -> do
      where_ (msg ^. MessageCourseIdent ==. val cid)
      when (isStudent role) $ do
        where_ (msg ^. MessageStudent ==. val uid)
      where_ $ notExists $
        from $ \readMsg -> do
          where_ (readMsg ^. ReadMessageMessage ==. msg ^. MessageId)
          where_ (readMsg ^. ReadMessageReader ==. val uid)
      return countRows

selectStaff :: MonadIO m => Text -> UserRole -> SqlPersistT m [Entity Profile]
selectStaff cid role = do
  select $
    from $ \(p `InnerJoin` r) -> do
      on (p ^. ProfileUser ==. r ^. RoleUser)
      where_ (r ^. RoleCourse ==. val cid)
      where_ (r ^. RoleRole ==. val role)
      return p

selectConversations :: MonadIO m => Text -> UserId -> SqlPersistT m [(Profile, Message, Profile, Bool)]
selectConversations cid authId = do
  xs <- select $ do
    from $ \((lm `InnerJoin` m `InnerJoin` s `InnerJoin` a) `LeftOuterJoin` rm) -> do
      on ((just (m ^. MessageId) ==. rm ?. ReadMessageMessage) &&. (just (val authId) ==. rm ?. ReadMessageReader))
      on (m ^. MessageStudent ==. s ^. ProfileUser)
      on (m ^. MessageAuthor ==. a ^. ProfileUser)
      on (lm ^. LastMessageMessage ==. m ^. MessageId)
      where_ (lm ^. LastMessageCourse ==. val cid)
      orderBy [desc (m ^. MessageDateTime)]
      return (s, m, a, isNothing (rm ?. ReadMessageMessage))
  return $ map (\(Entity _ s, Entity _ m, Entity _ a, Value isUnread) -> (s, m, a, isUnread)) xs
