module UserRole where

import Prelude
import Database.Persist.TH

data UserRole
  = RoleTeacher
  | RoleAssistant
  | RoleStudent
  deriving (Eq, Show, Read)

isTeacher :: UserRole -> Bool
isTeacher RoleTeacher = True
isTeacher _ = False

isAssistant :: UserRole -> Bool
isAssistant RoleAssistant = True
isAssistant _ = False

isStudent :: UserRole -> Bool
isStudent RoleStudent = True
isStudent _ = False

derivePersistField "UserRole"
