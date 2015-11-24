{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Types where

import           Data.Aeson
import           Data.Text      (Text)
import           Web.Spock.Safe

data AppConfig = AppConfig
    { acPort :: Int
    }

data AppState = AppState
    { asEnv :: Env
    }

type SessionVal = Maybe SessionId
type WebApp ctx = SpockCtxM ctx () SessionVal AppState ()
type Action ctx a = SpockActionCtx ctx () SessionVal AppState a


-- Interface for different data sources.
data DataSource = DataSource
    { findTask         :: TaskID -> IO ReviewTask
    , findTasksForUser :: UserID -> IO [ReviewTask]
    }

data Env = Env
    { eDataSource :: DataSource
    }

type UserID = Text
type TaskID = Text

data ReviewStatus = Waiting
                  | Reviewed
                  | Accepted
                  deriving (Show)

data ReviewTask = ReviewTask
    { rtContent :: Text
    , rtTaskId  :: TaskID
    , rtUserId  :: UserID
    } deriving (Show)

data PeerReview = PeerReview
    { prtaskId     :: TaskID
    , prComment    :: Text
    , prScore      :: Int
    , prReviewerId :: UserID
    , prStatus     :: ReviewStatus
    } deriving (Show)


instance ToJSON ReviewStatus where
    toJSON Waiting  = String "waiting"
    toJSON Reviewed = String "reviewed"
    toJSON Accepted = String "accepted"

instance ToJSON PeerReview where
    toJSON pr = object
        [ "taskId"     .= prtaskId pr
        , "comment"    .= prComment pr
        , "score"      .= prScore pr
        , "reviewerId" .= prReviewerId pr
        , "status"     .= prStatus pr
        ]

instance FromJSON ReviewStatus where
    parseJSON (String "waiting")  = pure Waiting
    parseJSON (String "reviewed") = pure Reviewed
    parseJSON (String "accepted") = pure Accepted
    parseJSON _ = mempty

instance FromJSON PeerReview where
    parseJSON (Object v) =
        PeerReview <$> v .: "taskId"
                   <*> v .: "comment"
                   <*> v .: "score"
                   <*> v .: "reviewerId"
                   <*> v .: "status"
    parseJSON _ = mempty
