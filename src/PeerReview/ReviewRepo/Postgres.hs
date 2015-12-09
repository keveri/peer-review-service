module PeerReview.ReviewRepo.Postgres
    ( repo
    , wipeDb
    ) where

import           Control.Monad              (void)
import           Data.Pool                  (Pool, createPool, withResource)
import           Data.String                (fromString)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             close, connect, execute_)

import           PeerReview.ReviewRepo.Transaction
import           PeerReview.Types

-- TODO: comments to the file
repo :: DBInfo -> IO ReviewRepo
repo dbi = do
    pool <- mkPoolAndInitDb dbi
    return $ ReviewRepo
        (saveReview pool)
        (findById pool)
        (findReviewsByUserId pool)
        (findCompleted pool)

wipeDb :: DBInfo -> IO ()
wipeDb dbi = do
    conn <- connect $ mkConnInfo dbi
    wipe <- readFile wipeFile
    void $ execute_ conn $ fromString wipe


wipeFile :: FilePath
wipeFile = "database/wipe.sql"

schemaFile :: FilePath
schemaFile = "database/schema.sql"

mkPoolAndInitDb :: DBInfo -> IO (Pool Connection)
mkPoolAndInitDb dbi = do
    connPool <- createPool createConn close
                           1   -- # sub-pools
                           60  -- unused resource timeout
                           100 -- max # of active connections
    withResource connPool createDB
    return connPool
  where
    createConn = connect $ mkConnInfo dbi


createDB :: Connection -> IO ()
createDB conn = do
    schema <- readFile schemaFile
    void $ execute_ conn $ fromString schema

mkConnInfo :: DBInfo -> ConnectInfo
mkConnInfo (DBInfo h p u pass n) =
    ConnectInfo h (fromIntegral p) u pass n
