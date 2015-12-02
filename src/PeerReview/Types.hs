{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Types where

import           Data.Aeson
import           Data.Map                             (Map)
import           Data.Pool                            (Pool)
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as T
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow   as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import qualified Database.PostgreSQL.Simple.ToRow     as PG
import           Web.Spock.Safe

data DBInfo = DBInfo
    { dbHost :: String
    , dbPort :: Int
    , dbUser :: String
    , dbPass :: String
    , dbName :: String
    }

data AppConfig = AppConfig
    { acPort :: Int
    , acDB   :: DBInfo
    }

data AppState = AppState
    { asEnv :: Env
    }

type SessionVal = Maybe SessionId
type WebApp ctx = SpockCtxM ctx () SessionVal AppState ()
type Action ctx a = SpockActionCtx ctx () SessionVal AppState a


type TaskSourceConfig = Map Text Text

-- Interface for different task sources.
data TaskSource = TaskSource
    { tsFindById    :: TaskID -> IO Task
    , tsFindForUser :: UserID -> IO [Task]
    , tsAll         :: IO [Task]
    }

data Env = Env
    { eTaskSource :: TaskSource
    , ePool       :: Pool PG.Connection
    }

type UserID = Text
type TaskID = Text

data ReviewStatus = Waiting
                  | Reviewed
                  | Accepted
                  deriving (Show)

data Task = Task
    { tId      :: TaskID
    , tUserId  :: UserID
    , tContent :: Maybe Text
    }

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
              "waiting"  -> return Waiting
              "reviewed" -> return Reviewed
              "accepted" -> return Accepted
              _          -> error "invalid status"

instance PG.FromRow ReviewStatus where
    fromRow = PG.field

instance PG.ToField ReviewStatus where
    toField Waiting  = PG.toField ("waiting"  :: Text)
    toField Reviewed = PG.toField ("reviewed" :: Text)
    toField Accepted = PG.toField ("accepted" :: Text)

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
