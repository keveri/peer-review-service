module PeerReview.Database
    ( mkPoolAndInitDb
    ) where

import           Control.Monad              (void)
import           Data.Pool                  (Pool, createPool, withResource)
import           Data.String                (fromString)
import           Database.PostgreSQL.Simple (ConnectInfo(..), Connection,
                                             connect, execute_, close)

import           PeerReview.Types

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
