{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Types where

import           Data.Aeson
import           Data.Map                             (Map)
import           Data.Text                            (Text)
import           Data.ByteString.Lazy.Internal
import qualified Data.Text.Encoding                   as T
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.FromRow   as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import qualified Database.PostgreSQL.Simple.ToRow     as PG

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

data ErrorMessage = ErrorMessage
    { emMessage  :: Text
    , emCode     :: Int
    }

instance ToJSON ErrorMessage where
    toJSON em = object
        [ "message" .= emMessage em
        , "code"    .= emCode em
        ]

type SubmissionRepoConfig = Map Text Text

-- Interface for different submission repos.
data SubmissionRepo = SubmissionRepo
    { srFindById     :: SubmissionID -> IO (Maybe SubmissionDetails)
    , srFindByTaskId :: TaskID -> IO [Submission]
    , srFindByUserId :: UserID -> IO [Submission]
    , srAll          :: IO [Submission]
    }

-- Interface for peer review repos.
data ReviewRepo = ReviewRepo
    { rrSave          :: PeerReview -> IO ()
    , rrFindById      :: PeerReviewID -> IO (Maybe PeerReview)
    , rrFindByUserId  :: UserID -> IO [PeerReview]
    }

-- Interface for APIClients.
data APIClient = APIClient
    { acGetResource :: String -> IO ByteString
    }

data Env = Env
    { eSubmissionRepo :: SubmissionRepo
    , eReviewRepo     :: ReviewRepo
    }

type UserID = Text
type TaskID = Text
type SubmissionID = Text
type Content = Text

type PeerReviewID = Int

data ReviewStatus = Waiting
                  | Reviewed
                  deriving (Show, Eq)

data Submission = Submission
    { sId      :: SubmissionID
    , sSender  :: UserID
    , sTid     :: TaskID
    } deriving (Show)

data SubmissionDetails = SubmissionDetails
    { sdId           :: SubmissionID
    , sdTid          :: TaskID
    , sdParticipants :: [UserID]
    , sdContent      :: Content
    } deriving (Show, Eq)

data PeerReview = PeerReview
    { prSubmissionId :: SubmissionID
    , prTaskId       :: TaskID
    , prComment      :: Text
    , prScore        :: Int
    , prReviewerId   :: UserID
    , prStatus       :: ReviewStatus
    } deriving (Show, Eq)


instance ToJSON ReviewStatus where
    toJSON Waiting  = String "waiting"
    toJSON Reviewed = String "reviewed"

instance ToJSON PeerReview where
    toJSON pr = object
        [ "submissionId" .= prSubmissionId pr
        , "comment"      .= prComment pr
        , "score"        .= prScore pr
        , "reviewerId"   .= prReviewerId pr
        , "status"       .= prStatus pr
        ]

instance PG.FromField ReviewStatus where
    fromField f Nothing  = PG.returnError PG.UnexpectedNull f ""
    fromField _ (Just b) =
        case T.decodeUtf8 b of
              "waiting"  -> return Waiting
              "reviewed" -> return Reviewed
              _          -> error "invalid status"

instance PG.FromRow ReviewStatus where
    fromRow = PG.field

instance PG.ToField ReviewStatus where
    toField Waiting  = PG.toField ("waiting"  :: Text)
    toField Reviewed = PG.toField ("reviewed" :: Text)

instance PG.ToRow PeerReview where
    toRow (PeerReview subId tId comment score reviewerId status) =
        PG.toRow (subId, tId, comment, score, reviewerId, status)

instance PG.FromRow PeerReview where
    fromRow =
        PeerReview <$> PG.field
                   <*> PG.field
                   <*> PG.field
                   <*> PG.field
                   <*> PG.field
                   <*> PG.field
