module AssignmentAction where

import Prelude
import Yesod.Core.Dispatch

data AssignmentAction
  = AssignmentLock
  | AssignmentUnlock
  | AssignmentStart
  | AssignmentStop
  | AssignmentExtraDay
  | AssignmentSkipDay
  | AssignmentReset
  deriving (Eq, Show, Read)

instance PathPiece AssignmentAction where
  toPathPiece AssignmentLock      = "lock"
  toPathPiece AssignmentUnlock    = "unlock"
  toPathPiece AssignmentStart     = "start"
  toPathPiece AssignmentStop      = "stop"
  toPathPiece AssignmentExtraDay  = "extra-day"
  toPathPiece AssignmentSkipDay   = "skip-day"
  toPathPiece AssignmentReset     = "reset"

  fromPathPiece "lock"      = Just AssignmentLock
  fromPathPiece "unlock"    = Just AssignmentUnlock
  fromPathPiece "start"     = Just AssignmentStart
  fromPathPiece "stop"      = Just AssignmentStop
  fromPathPiece "extra-day" = Just AssignmentExtraDay
  fromPathPiece "skip-day"  = Just AssignmentSkipDay
  fromPathPiece "reset"     = Just AssignmentReset
  fromPathPiece _ = Nothing

