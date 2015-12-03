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


type SubmissionRepoConfig = Map Text Text

-- Interface for different submission repos.
data SubmissionRepo = SubmissionRepo
    { tsFindById    :: TaskID -> IO Submission
    , tsFindForUser :: UserID -> IO [Submission]
    , tsAll         :: IO [Submission]
    }

data Env = Env
    { eSubmissionRepo :: SubmissionRepo
    , ePool           :: Pool PG.Connection
    }

type UserID = Text
type TaskID = Text
type SubmissionID = Text
type Content = Text

data ReviewStatus = Waiting
                  | Reviewed
                  | Accepted
                  deriving (Show)

data Submission = Submission
    { sId      :: SubmissionID
    , sUid     :: UserID
    , sTid     :: TaskID
    , sContent :: Maybe Content
    }

data PeerReview = PeerReview
    { prSubmissionId :: SubmissionID
    , prComment      :: Text
    , prScore        :: Int
    , prReviewerId   :: UserID
    , prStatus       :: ReviewStatus
    } deriving (Show)


instance ToJSON ReviewStatus where
    toJSON Waiting  = String "waiting"
    toJSON Reviewed = String "reviewed"
    toJSON Accepted = String "accepted"

instance ToJSON PeerReview where
    toJSON pr = object
        [ "submissionId" .= prSubmissionId pr
        , "comment"      .= prComment pr
        , "score"        .= prScore pr
        , "reviewerId"   .= prReviewerId pr
        , "status"       .= prStatus pr
        ]

instance FromJSON ReviewStatus where
    parseJSON (String "waiting")  = pure Waiting
    parseJSON (String "reviewed") = pure Reviewed
    parseJSON (String "accepted") = pure Accepted
    parseJSON _ = mempty

instance FromJSON PeerReview where
    parseJSON (Object v) =
        PeerReview <$> v .: "submissionId"
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
    toRow (PeerReview submissionId comment score reviewerId status) =
        PG.toRow (submissionId, comment, score, reviewerId, status)

instance PG.FromRow PeerReview where
    fromRow =
        PeerReview <$> PG.field
                   <*> PG.field
                   <*> PG.field
                   <*> PG.field
                   <*> PG.field
