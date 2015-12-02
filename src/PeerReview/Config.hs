{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Config
    ( readAppConfig
    , readTaskSourceConfig
    ) where

import           Data.Aeson              (decode)
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Configurator       as C
import           Data.Configurator.Types

import           PeerReview.Types

-- Read config file.
readAppConfig :: FilePath -> IO AppConfig
readAppConfig cfgFile = do
    cfg      <- C.load [C.Required cfgFile]
    port     <- C.require cfg "port"
    database <- parseDB cfg
    return $ AppConfig port database

-- Read config as JSON for now.
-- Format: { "key": "value" }
readTaskSourceConfig :: FilePath -> IO TaskSourceConfig
readTaskSourceConfig fp = do
    let errorMsg = "Couldn't parse task source config."
    mObj <- decode <$> LB.readFile fp
    maybe (fail errorMsg) return mObj


-- Parse database related information.
parseDB :: Config -> IO DBInfo
parseDB cfg = do
    host <- C.require cfg "db.host"
    port <- C.require cfg "db.port"
    user <- C.require cfg "db.user"
    pass <- C.require cfg "db.pass"
    name <- C.require cfg "db.name"
    return $ DBInfo host port user pass name
