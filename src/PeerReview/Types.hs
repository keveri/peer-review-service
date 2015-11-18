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


type Email = Text

data Task = Task
    { tContent :: Text
    , tId      :: Int
    } deriving (Show, Generic)
instance FromJSON Task
instance ToJSON Task

data PeerReview = PeerReview
    { prTask          :: Task
    , prComment       :: Text
    , prScore         :: Int
    , prReviewerEmail :: Email
    } deriving (Show, Generic)
instance FromJSON PeerReview
instance ToJSON PeerReview
