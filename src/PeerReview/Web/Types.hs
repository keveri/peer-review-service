{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Web.Types where

import           Data.Aeson
import           Data.Text        (Text)
import           Web.Spock.Safe

import           PeerReview.Types

data AppState = AppState
    { asEnv :: Env
    }

type SessionVal = Maybe SessionId
type WebApp ctx = SpockCtxM ctx () SessionVal AppState ()
type Action ctx a = SpockActionCtx ctx () SessionVal AppState a

data CreateReviewBody = CreateReviewBody
    { crbUid :: UserID
    }

instance FromJSON CreateReviewBody where
    parseJSON (Object o) =
        CreateReviewBody <$> o .: "userID"
    parseJSON _ = mempty

data ReviewResponse = ReviewResponse
    { rwId           :: PeerReviewID
    , rwSubmissionId :: SubmissionID
    , rwTaskId       :: TaskID
    , rwComment      :: Text
    , rwScore        :: Int
    , rwReviewerId   :: UserID
    , rwStatus       :: ReviewStatus
    }

instance ToJSON ReviewResponse where
    toJSON rw = object
        [ "id"           .= rwId rw
        , "submissionId" .= rwSubmissionId rw
        , "taskId"       .= rwTaskId rw
        , "comment"      .= rwComment rw
        , "score"        .= rwScore rw
        , "reviewerId"   .= rwReviewerId rw
        , "status"       .= rwStatus rw
        ]
