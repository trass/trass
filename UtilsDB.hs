{-# LANGUAGE ScopedTypeVariables #-}
module UtilsDB where

import Prelude
import Model
import UserRole
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import Database.Esqueleto
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import SubmissionStatus

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

selectConversations :: MonadIO m => Text -> UserId -> SqlPersistT m [(Profile, Message, Profile, UserRole, Bool)]
selectConversations cid authId = do
  xs <- select $
    from $ \((lm `InnerJoin` m `InnerJoin` s `InnerJoin` a) `LeftOuterJoin` rm `LeftOuterJoin` r) -> do
      on ((a ^. ProfileUser ==. r ^. RoleUser) &&. (r ^. RoleCourse ==. val cid))
      on ((just (m ^. MessageId) ==. rm ?. ReadMessageMessage) &&. (just (val authId) ==. rm ?. ReadMessageReader))
      on (m ^. MessageStudent ==. s ^. ProfileUser)
      on (m ^. MessageAuthor ==. a ^. ProfileUser)
      on (lm ^. LastMessageMessage ==. m ^. MessageId)
      where_ (lm ^. LastMessageCourse ==. val cid)
      orderBy [desc (m ^. MessageDateTime)]
      return (s, m, a, r ^. RoleRole, isNothing (rm ?. ReadMessageMessage))
  return $ map (\(Entity _ s, Entity _ m, Entity _ a, Value r, Value isUnread) -> (s, m, a, r, isUnread)) xs

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

getSubmissionsCountByStudent :: MonadIO m => CourseId -> UserId -> SqlPersistT m Int64
getSubmissionsCountByStudent cid uid = do
  [Value n] <- select $
    from $ \s -> do
      where_ (s ^. SubmissionCourse ==. val cid)
      where_ (s ^. SubmissionAuthor ==. val uid)
      return countRows
  return n

getSubmissionsByStudent :: MonadIO m => Int64 -> Int64 -> CourseId -> UserId -> SqlPersistT m [(Entity Submission, Entity Assignment, Entity Section)]
getSubmissionsByStudent perPage pageNo cid uid = do
  select $
    from $ \(s `InnerJoin` a `InnerJoin` se) -> do
      on (a ^. AssignmentSection ==. se ^. SectionId)
      on (s ^. SubmissionAssignment ==. a ^. AssignmentId)
      where_ (s ^. SubmissionCourse ==. val cid)
      where_ (s ^. SubmissionAuthor ==. val uid)
      orderBy [desc (s ^. SubmissionUpdatedAt)]
      page perPage pageNo
      return (s, a, se)

getSubmissionCountsByStatus :: MonadIO m => CourseId -> SqlPersistT m [(SubmissionStatus, Int)]
getSubmissionCountsByStatus cid = do
  xs <- select $
    from $ \s -> do
      where_ (s ^. SubmissionCourse ==. val cid)
      groupBy (s ^. SubmissionStatus)
      return (s ^. SubmissionStatus, countRows)
  return $ map (\(Value s, Value n) -> (s, n)) xs

getSubmissionsByStatus :: MonadIO m => Int64 -> Int64 -> CourseId -> SubmissionStatus -> SqlPersistT m [(Entity Submission, Entity Assignment, Entity Section, Entity Profile)]
getSubmissionsByStatus perPage pageNo cid status = do
  select $
    from $ \(s `InnerJoin` a `InnerJoin` se `InnerJoin` p) -> do
      on (s ^. SubmissionAuthor ==. p ^. ProfileUser)
      on (a ^. AssignmentSection ==. se ^. SectionId)
      on (s ^. SubmissionAssignment ==. a ^. AssignmentId)
      where_ (s ^. SubmissionCourse ==. val cid)
      where_ (s ^. SubmissionStatus ==. val status)
      orderBy [desc (s ^. SubmissionUpdatedAt)]
      page perPage pageNo
      return (s, a, se, p)

page :: Esqueleto query expr backend => Int64 -> Int64 -> query ()
page perPage pageNo = do
  limit perPage
  offset (perPage * (pageNo - 1))

getStudentCoursePoints :: MonadIO m => Int64 -> Int64 -> CourseId -> UserId -> SqlPersistT m [(Entity CoursePoints, Maybe (Entity Assignment), Maybe (Entity Section), Maybe (Entity Profile))]
getStudentCoursePoints perPage pageNo cid uid = do
  select $
    from $ \(cp `LeftOuterJoin` s `LeftOuterJoin` a `LeftOuterJoin` se `LeftOuterJoin` p) -> do
      on (cp ^. CoursePointsAuthor ==. p ?. ProfileUser)
      on (cp ^. CoursePointsSection ==. se ?. SectionId)
      on (s ?. SubmissionAssignment ==. a ?. AssignmentId)
      on (cp ^. CoursePointsSubmission ==. s ?. SubmissionId)
      where_ (cp ^. CoursePointsCourse ==. val cid)
      where_ (cp ^. CoursePointsStudent ==. val uid)
      orderBy [desc (cp ^. CoursePointsCreatedAt)]
      page perPage pageNo
      return (cp, a, se, p)

getStudentCoursePointsSum :: MonadIO m => CourseId -> UserId -> SqlPersistT m Int
getStudentCoursePointsSum cid uid = do
  [Value n] <- select $
    from $ \cp -> do
      where_ (cp ^. CoursePointsCourse ==. val cid)
      where_ (cp ^. CoursePointsStudent ==. val uid)
      return $ sum_ (cp ^. CoursePointsPoints)
  return $ fromMaybe 0 n

getStudentCoursePointsSums :: MonadIO m => CourseId -> [UserId] -> SqlPersistT m [(UserId, Int)]
getStudentCoursePointsSums cid uids = do
  xs <- select $
    from $ \cp -> do
      where_ (cp ^. CoursePointsCourse ==. val cid)
      where_ (cp ^. CoursePointsStudent `in_` valList uids)
      groupBy (cp ^. CoursePointsStudent)
      return $ (cp ^. CoursePointsStudent, sum_ (cp ^. CoursePointsPoints))
  return $ map (\(Value i, Value s) -> (i, fromMaybe 0 s)) xs

getStudentCoursePointsCount :: MonadIO m => CourseId -> UserId -> SqlPersistT m Int64
getStudentCoursePointsCount cid uid = do
  [Value n] <- select $
    from $ \cp -> do
      where_ (cp ^. CoursePointsCourse ==. val cid)
      where_ (cp ^. CoursePointsStudent ==. val uid)
      return countRows
  return n

