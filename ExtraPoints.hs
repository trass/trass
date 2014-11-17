module ExtraPoints where

import Prelude
import Database.Persist.TH

data ExtraPointsType
  = ExtraPointsForCourse
  | ExtraPointsForSection
  | ExtraPointsForAssignment
  | ExtraPointsForSubmission
  deriving (Eq, Ord, Show, Read)
derivePersistField "ExtraPointsType"

data ExtraPointsPredefined
  = ExtraPointsPredefinedClassActivity
  | ExtraPointsPredefinedTeacherMistake
  | ExtraPointsPredefinedElegantSolution
  | ExtraPointsPredefinedMissedDeadline
  | ExtraPointsPredefinedPlagiarism
  deriving (Eq, Ord, Show, Read)
derivePersistField "ExtraPointsPredefined"

