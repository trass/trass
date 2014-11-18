{-# LANGUAGE ScopedTypeVariables #-}
module UtilsDB where

import Prelude
import Model
import UserRole
import Data.Int
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time
import Database.Esqueleto
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import SubmissionStatus
import Achievement

getUserCourses :: MonadIO m => UserId -> SqlPersistT m [Entity Course]
getUserCourses uid = do
  select $
    from $ \(r `InnerJoin` c) -> do
      on (r ^. RoleCourse ==. c ^. CourseId)
      where_ (r ^. RoleUser ==. val uid)
      return c

countUnreadMessages :: MonadIO m => CourseId -> UserId -> UserRole -> SqlPersistT m Int
countUnreadMessages cid uid role = liftM (unValue . head) $ do
  select $
    from $ \msg -> do
      where_ (msg ^. MessageCourse ==. val cid)
      when (isStudent role) $ do
        where_ (msg ^. MessageStudent ==. val uid)
      where_ $ notExists $
        from $ \readMsg -> do
          where_ (readMsg ^. ReadMessageMessage ==. msg ^. MessageId)
          where_ (readMsg ^. ReadMessageReader ==. val uid)
      return countRows

selectStaff :: MonadIO m => CourseId -> UserRole -> SqlPersistT m [Entity Profile]
selectStaff cid role = do
  select $
    from $ \(p `InnerJoin` r) -> do
      on (p ^. ProfileUser ==. r ^. RoleUser)
      where_ (r ^. RoleCourse ==. val cid)
      where_ (r ^. RoleRole ==. val role)
      return p

selectConversations :: MonadIO m => CourseId -> UserId -> SqlPersistT m [(Profile, Message, Profile, UserRole, Bool)]
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

getSubmissionsByStudent :: MonadIO m => Int64 -> Int64 -> CourseId -> UserId -> SqlPersistT m [Submission]
getSubmissionsByStudent perPage pageNo cid uid = liftM (map entityVal) $ do
  select $
    from $ \s -> do
      where_ (s ^. SubmissionCourse ==. val cid)
      where_ (s ^. SubmissionAuthor ==. val uid)
      orderBy [desc (s ^. SubmissionUpdatedAt)]
      page perPage pageNo
      return s

getSubmissionCountsByStatus :: MonadIO m => CourseId -> SqlPersistT m [(SubmissionStatus, Int)]
getSubmissionCountsByStatus cid = do
  xs <- select $
    from $ \s -> do
      where_ (s ^. SubmissionCourse ==. val cid)
      groupBy (s ^. SubmissionStatus)
      return (s ^. SubmissionStatus, countRows)
  return $ map (\(Value s, Value n) -> (s, n)) xs

getSubmissionsByStatus :: MonadIO m => Int64 -> Int64 -> CourseId -> SubmissionStatus -> SqlPersistT m [Submission]
getSubmissionsByStatus perPage pageNo cid status = liftM (map entityVal) $ do
  select $
    from $ \s -> do
      where_ (s ^. SubmissionCourse ==. val cid)
      where_ (s ^. SubmissionStatus ==. val status)
      orderBy [desc (s ^. SubmissionUpdatedAt)]
      page perPage pageNo
      return s

getCourseSubmissionsCount :: MonadIO m => CourseId -> Maybe (Either UserId UserId) -> Maybe SubmissionStatus -> Maybe AssignmentId -> SqlPersistT m Int64
getCourseSubmissionsCount cid muid mstatus maid = do
  [Value n] <- select $
    from $ \s -> do
      where_ (s ^. SubmissionCourse ==. val cid)
      case muid of
        Just (Left uid)  -> where_ (s ^. SubmissionAuthor !=. val uid)
        Just (Right uid) -> where_ (s ^. SubmissionAuthor ==. val uid)
        Nothing -> return ()
      mwhere_ (s ^. SubmissionStatus ==.) mstatus
      mwhere_ (s ^. SubmissionAssignment ==.) maid
      return countRows
  return n
  where
    mwhere_ f (Just v) = where_ (f $ val v)
    mwhere_ _ Nothing  = return ()

getCourseSubmissions :: MonadIO m => Int64 -> Int64 -> CourseId -> Maybe (Either UserId UserId) -> Maybe SubmissionStatus -> Maybe AssignmentId -> SqlPersistT m [Entity Submission]
getCourseSubmissions perPage pageNo cid muid mstatus maid = do
  select $
    from $ \s -> do
      where_ (s ^. SubmissionCourse ==. val cid)
      case muid of
        Just (Left uid)  -> where_ (s ^. SubmissionAuthor !=. val uid)
        Just (Right uid) -> where_ (s ^. SubmissionAuthor ==. val uid)
        Nothing -> return ()
      mwhere_ (s ^. SubmissionStatus ==.) mstatus
      mwhere_ (s ^. SubmissionAssignment ==.) maid
      orderBy [desc (s ^. SubmissionUpdatedAt)]
      page perPage pageNo
      return s
  where
    mwhere_ f (Just v) = where_ (f $ val v)
    mwhere_ _ Nothing  = return ()

page :: Esqueleto query expr backend => Int64 -> Int64 -> query ()
page perPage pageNo = do
  limit perPage
  offset (perPage * (pageNo - 1))

getStudentEvents ::
  MonadIO m => Int64 -> Int64 -> CourseId -> UserId ->
  SqlPersistT m [(Entity Event, Maybe ExtraPoints, Maybe Achievement, Maybe Assignment, Maybe Section, Maybe Profile, Maybe UserRole)]
getStudentEvents perPage pageNo cid uid = do
  xs <- select $
    from $ \(e `LeftOuterJoin` ep `LeftOuterJoin` ac `LeftOuterJoin` as `LeftOuterJoin` s `LeftOuterJoin` p `LeftOuterJoin` r) -> do
      on (e ^. EventCreatedBy   ==. r ?. RoleUser)
      on (e ^. EventCreatedBy   ==. p ?. ProfileUser)
      on (e ^. EventSection     ==. s ?. SectionId)
      on (e ^. EventAssignment  ==. as ?. AssignmentId)
      on (e ^. EventAchievement ==. ac ?. AchievementId)
      on (e ^. EventExtraPoints ==. ep ?. ExtraPointsId)
      where_ (e ^. EventCourse ==. val cid)
      where_ (e ^. EventStudent ==. val uid)
      orderBy [desc (e ^. EventDateTime)]
      page perPage pageNo
      return (e, ep, ac, as, s, p, r ?. RoleRole)
  return $ map (\(e, ep, ac, as, s, p, r) -> (e, entityVal <$> ep, entityVal <$> ac, entityVal <$> as, entityVal <$> s, entityVal <$> p, unValue r)) xs

getStudentEventsCount :: MonadIO m => CourseId -> UserId -> SqlPersistT m Int64
getStudentEventsCount cid uid = do
  [Value n] <- select $
    from $ \e -> do
      where_ (e ^. EventCourse ==. val cid)
      where_ (e ^. EventStudent ==. val uid)
      return countRows
  return n

getStudentCoursePointsSum :: MonadIO m => CourseId -> UserId -> SqlPersistT m Int
getStudentCoursePointsSum cid uid = do
  [Value n] <- select $
    from $ \(e `LeftOuterJoin` ep `LeftOuterJoin` as) -> do
      on (e ^. EventAssignment ==. as ?. AssignmentId)
      on (e ^. EventExtraPoints ==. ep ?. ExtraPointsId)
      where_ (e ^. EventCourse ==. val cid)
      where_ (e ^. EventStudent ==. val uid)
      return $ sum_ $ coalesceDefault [as ?. AssignmentPoints] (ep ?. ExtraPointsPoints)
  return $ fromMaybe 0 n

getStudentUnreadCoursePointsSum :: MonadIO m => CourseId -> UserId -> SqlPersistT m (Maybe Int)
getStudentUnreadCoursePointsSum cid uid = do
  [Value n] <- select $
    from $ \(e `LeftOuterJoin` ep `LeftOuterJoin` as) -> do
      on (e ^. EventAssignment ==. as ?. AssignmentId)
      on (e ^. EventExtraPoints ==. ep ?. ExtraPointsId)
      where_ (e ^. EventCourse ==. val cid)
      where_ (e ^. EventStudent ==. val uid)
      where_ (e ^. EventIsRead ==. val False)
      return $ sum_ $ coalesceDefault [as ?. AssignmentPoints] (ep ?. ExtraPointsPoints)
  return n

getStudentAchievements :: MonadIO m => CourseId -> UserId -> AchievementType -> SqlPersistT m [(Achievement, Int)]
getStudentAchievements cid uid t = do
  xs <- select $
    from $ \(aa `InnerJoin` a) -> do
      on (aa ^. AwardedAchievementAchievement ==. a ^. AchievementId)
      where_ (aa ^. AwardedAchievementCourse ==. val cid)
      where_ (aa ^. AwardedAchievementStudent ==. val uid)
      where_ (a ^. AchievementType ==. val t)
      return (a, aa ^. AwardedAchievementTimes)
  return $ map (\(Entity _ a, Value n) -> (a, n)) xs

getStudentAchievementsTotal :: MonadIO m => CourseId -> UserId -> SqlPersistT m [(AchievementType, Int)]
getStudentAchievementsTotal cid uid = do
  xs <- select $
    from $ \(aa `InnerJoin` a) -> do
      on (aa ^. AwardedAchievementAchievement ==. a ^. AchievementId)
      where_ (aa ^. AwardedAchievementCourse ==. val cid)
      where_ (aa ^. AwardedAchievementStudent ==. val uid)
      groupBy (a ^. AchievementType)
      return (a ^. AchievementType, sum_ (aa ^. AwardedAchievementTimes))
  return $ map (\(Value t, Value n) -> (t, fromMaybe 0 n)) xs

getConversationMessages :: MonadIO m => CourseId -> UserId -> UserId -> SqlPersistT m [(Entity Message, Bool)]
getConversationMessages cid uid reader = do
  xs <- select $
    from $ \(m `LeftOuterJoin` rm) -> do
      on ((just (m ^. MessageId) ==. rm ?. ReadMessageMessage) &&. (rm ?. ReadMessageReader ==. just (val reader)))
      where_ (m ^. MessageCourse ==. val cid)
      where_ (m ^. MessageStudent ==. val uid)
      orderBy [asc (m ^. MessageDateTime)]
      return $ (m, isNothing (rm ?. ReadMessageMessage))
  return $ map (\(m, Value r) -> (m, r)) xs

getStudentGroup :: MonadIO m => CourseId -> UserId -> SqlPersistT m (Maybe (Entity Group))
getStudentGroup cid uid = liftM listToMaybe $ do
  select $
    from $ \(gm `InnerJoin` g) -> do
      on (gm ^. GroupMemberGroup ==. g ^. GroupId)
      where_ (gm ^. GroupMemberCourse ==. val cid)
      where_ (gm ^. GroupMemberStudent ==. val uid)
      return g

