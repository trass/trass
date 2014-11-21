{-# LANGUAGE ScopedTypeVariables #-}
module UtilsDB where

import Prelude
import Model
import UserRole
import Data.Int
import Data.Maybe (listToMaybe)
import Data.Time
import Database.Esqueleto
import Control.Monad
import Control.Monad.IO.Class (MonadIO)

getUserCourses :: MonadIO m => UserId -> SqlPersistT m [Entity Course]
getUserCourses uid = do
  select $
    from $ \(r `InnerJoin` c) -> do
      on (r ^. RoleCourse ==. c ^. CourseId)
      where_ (r ^. RoleUser ==. val uid)
      return c

selectStaff :: MonadIO m => CourseId -> UserRole -> SqlPersistT m [Entity Profile]
selectStaff cid role = do
  select $
    from $ \(p `InnerJoin` r) -> do
      on (p ^. ProfileUser ==. r ^. RoleUser)
      where_ (r ^. RoleCourse ==. val cid)
      where_ (r ^. RoleRole ==. val role)
      return p

selectGroupMembers :: MonadIO m => Maybe GroupId -> SqlPersistT m [Profile]
selectGroupMembers Nothing = liftM (map entityVal) $ do
  select $
    from $ \((r `InnerJoin` p) `LeftOuterJoin` gm) -> do
      on (just (r ^. RoleUser) ==. gm ?. GroupMemberStudent)
      on (r ^. RoleUser ==. p ^. ProfileUser)
      where_ (r ^. RoleRole ==. val (RoleStudent))
      where_ (isNothing (gm ?. GroupMemberStudent))
      return p
selectGroupMembers (Just gid) = liftM (map entityVal) $ do
  select $
    from $ \(gm `InnerJoin` p) -> do
      on (gm ^. GroupMemberStudent ==. p ^. ProfileUser)
      where_ (gm ^. GroupMemberGroup ==. val gid)
      return p

isSectionLocked :: MonadIO m => SectionId -> SqlPersistT m Bool
isSectionLocked sid = do
  lockedAssignments <- select $
    from $ \a -> do
      where_ (a ^. AssignmentSection ==. val sid)
      where_ (a ^. AssignmentLocked ==. val True)
      limit 1
      return a
  if null lockedAssignments
    then return False
    else do
      subsections <- select $
        from $ \s -> do
          where_ (s ^. SectionParent ==. just (val sid))
          limit 1
          return s
      unlockedAssignments <- select $
        from $ \a -> do
          where_ (a ^. AssignmentSection ==. val sid)
          where_ (a ^. AssignmentLocked ==. val False)
          limit 1
          return a
      return $ null subsections && null unlockedAssignments

getSectionAssignmentInfo :: MonadIO m => SectionId -> SqlPersistT m (Maybe Int, Maybe Int64, Maybe UTCTime, Maybe UTCTime)
getSectionAssignmentInfo sid = do
  [(Value p, Value d, Value s, Value e)] <- select $ do
    from $ \a -> do
      where_ (a ^. AssignmentSection ==. val sid)
      return $
        ( sum_ (coalesceDefault [a ^. AssignmentPoints] (val 0))
        , joinV $ min_ (a ^. AssignmentDuration)
        , joinV $ min_ (a ^. AssignmentStartedAt)
        , joinV $ min_ (a ^. AssignmentEndingAt)
        )
  return (p, d, s, e)

page :: Esqueleto query expr backend => Int64 -> Int64 -> query ()
page perPage pageNo = do
  limit perPage
  offset (perPage * (pageNo - 1))

getStudentGroup :: MonadIO m => CourseId -> UserId -> SqlPersistT m (Maybe (Entity Group))
getStudentGroup cid uid = liftM listToMaybe $ do
  select $
    from $ \(gm `InnerJoin` g) -> do
      on (gm ^. GroupMemberGroup ==. g ^. GroupId)
      where_ (gm ^. GroupMemberCourse ==. val cid)
      where_ (gm ^. GroupMemberStudent ==. val uid)
      return g

