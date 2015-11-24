{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Types where

import           Data.Aeson
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as T
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow   as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import qualified Database.PostgreSQL.Simple.ToRow     as PG
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


instance PG.FromField ReviewStatus where
    fromField f Nothing  = PG.returnError PG.UnexpectedNull f ""
    fromField _ (Just b) =
        case T.decodeUtf8 b of
              "WAITING"  -> return Waiting
              "REVIEWED" -> return Reviewed
              "ACCEPTED" -> return Accepted
              _          -> error "invalid status"

instance PG.FromRow ReviewStatus where
    fromRow = PG.field

instance PG.ToField ReviewStatus where
    toField Waiting  = PG.toField ("WAITING"  :: Text)
    toField Reviewed = PG.toField ("REVIEWED" :: Text)
    toField Accepted = PG.toField ("ACCEPTED" :: Text)

instance PG.ToRow PeerReview where
    toRow (PeerReview taskId comment score reviewerId status) =
        PG.toRow (taskId, comment, score, reviewerId, status)

instance PG.FromRow PeerReview where
    fromRow =
        PeerReview <$> PG.field
                   <*> PG.field
                   <*> PG.field
                   <*> PG.field
                   <*> PG.field
