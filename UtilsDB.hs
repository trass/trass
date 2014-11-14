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
