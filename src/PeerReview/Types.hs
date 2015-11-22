{-# LANGUAGE DeriveGeneric #-}
module PeerReview.Types where

import           Data.Aeson
import           Data.Text      (Text)
import           GHC.Generics

import           Web.Spock.Safe

data AppConfig = AppConfig
    { acPort :: Int
    }

data AppState = AppState
    { foo :: Int
    }

type SessionVal = Maybe SessionId
type App ctx = SpockCtxM ctx () SessionVal AppState ()
type Action ctx a = SpockActionCtx ctx () SessionVal AppState a


type UserID = Text
type TaskID = Text

data ReviewStatus = Waiting
                  | Reviewed
                  | Accepted
                  deriving (Show, Generic)
instance FromJSON ReviewStatus
instance ToJSON ReviewStatus

data ReviewTask = ReviewTask
    { rtContent :: Text
    , rtTaskID  :: TaskID
    , rtUserID  :: UserID
    } deriving (Show, Generic)
instance FromJSON ReviewTask
instance ToJSON ReviewTask

data PeerReview = PeerReview
    { prTask       :: ReviewTask
    , prComment    :: Text
    , prScore      :: Int
    , prReviewerID :: UserID
    , prReviewed   :: ReviewStatus
    } deriving (Show, Generic)
instance FromJSON PeerReview
instance ToJSON PeerReview
