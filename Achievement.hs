module Achievement where

import Prelude
import Database.Persist.TH

data AchievementType
  = AchievementGold
  | AchievementSilver
  | AchievementBronze
  | AchievementSecret
  deriving (Eq, Ord, Show, Read)
derivePersistField "AchievementType"

isAchievementGold :: AchievementType -> Bool
isAchievementGold AchievementGold = True
isAchievementGold _ = False

isAchievementSilver :: AchievementType -> Bool
isAchievementSilver AchievementSilver = True
isAchievementSilver _ = False

isAchievementBronze :: AchievementType -> Bool
isAchievementBronze AchievementBronze = True
isAchievementBronze _ = False

isAchievementSecret :: AchievementType -> Bool
isAchievementSecret AchievementSecret = True
isAchievementSecret _ = False

data AchievementPredefined
  = AchievementPredefinedHelloWorld
  | AchievementPredefinedAcceptable
  | AchievementPredefinedBugfix
  | AchievementPredefinedUpstairs
  | AchievementPredefinedKingOfTheHill
  | AchievementPredefinedChampion
  | AchievementPredefinedEarlyBird
  | AchievementPredefinedIDon'tWantSleep
  | AchievementPredefinedIDon'tNeedSleep
  | AchievementPredefinedProductiveInsomnia
  | AchievementPredefinedNightAndDay
  | AchievementPredefinedSocial
  | AchievementPredefinedRattlemouth
  | AchievementPredefinedSpamBot
  deriving (Eq, Ord, Show, Read)
derivePersistField "AchievementPredefined"

achievementPredefinedType :: AchievementPredefined -> AchievementType
achievementPredefinedType _ = AchievementBronze

