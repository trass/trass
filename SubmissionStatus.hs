module SubmissionStatus where

import Prelude
import Yesod.Core.Dispatch
import Database.Persist.TH

data SubmissionStatus
  = SubmissionSubmitted
  | SubmissionInvalid
  | SubmissionCompileError
  | SubmissionTestsFailed
  | SubmissionTestsPassed
  | SubmissionInReview
  | SubmissionRejected
  | SubmissionAccepted
  | SubmissionErrored
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PathPiece SubmissionStatus where
  toPathPiece SubmissionSubmitted     = "submitted"
  toPathPiece SubmissionInvalid       = "invalid"
  toPathPiece SubmissionCompileError  = "compile-error"
  toPathPiece SubmissionTestsFailed   = "tests-failed"
  toPathPiece SubmissionTestsPassed   = "tests-passed"
  toPathPiece SubmissionInReview      = "in-review"
  toPathPiece SubmissionRejected      = "rejected"
  toPathPiece SubmissionAccepted      = "accepted"
  toPathPiece SubmissionErrored       = "errored"

  fromPathPiece "submitted"     = Just SubmissionSubmitted
  fromPathPiece "invalid"       = Just SubmissionInvalid
  fromPathPiece "compile-error" = Just SubmissionCompileError
  fromPathPiece "tests-failed"  = Just SubmissionTestsFailed
  fromPathPiece "tests-passed"  = Just SubmissionTestsPassed
  fromPathPiece "in-review"     = Just SubmissionInReview
  fromPathPiece "rejected"      = Just SubmissionRejected
  fromPathPiece "accepted"      = Just SubmissionAccepted
  fromPathPiece "errored"       = Just SubmissionErrored
  fromPathPiece _ = Nothing

doesNeedAction :: SubmissionStatus -> Bool
doesNeedAction SubmissionTestsPassed = True
doesNeedAction SubmissionInReview    = True
doesNeedAction SubmissionErrored     = True
doesNeedAction _ = False

derivePersistField "SubmissionStatus"
