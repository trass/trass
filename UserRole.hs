module UserRole where

import Prelude
import Yesod.Core.Dispatch
import Database.Persist.TH

data UserRole
  = RoleTeacher
  | RoleAssistant
  | RoleStudent
  deriving (Eq, Show, Read)

instance PathPiece UserRole where
  toPathPiece RoleTeacher   = "teacher"
  toPathPiece RoleAssistant = "assistant"
  toPathPiece RoleStudent   = "student"

  fromPathPiece "teacher"   = Just RoleTeacher
  fromPathPiece "assistant" = Just RoleAssistant
  fromPathPiece "student"   = Just RoleStudent
  fromPathPiece _ = Nothing

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
