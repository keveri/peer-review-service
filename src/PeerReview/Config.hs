{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Config
    ( readAppConfig
    , readTaskSourceConfig
    ) where

import qualified Data.Configurator       as C
import           Data.Configurator.Types
import qualified Data.Map                as M (empty)

import           PeerReview.Types

-- Read config file.
readAppConfig :: FilePath -> IO AppConfig
readAppConfig cfgFile = do
    cfg      <- C.load [C.Required cfgFile]
    port     <- C.require cfg "port"
    database <- parseDB cfg
    return $ AppConfig port database

-- TODO: Implement config file reading.
readTaskSourceConfig :: FilePath -> IO TaskSourceConfig
readTaskSourceConfig _ = return M.empty


-- Parse database related information.
parseDB :: Config -> IO DBInfo
parseDB cfg = do
    host <- C.require cfg "db.host"
    port <- C.require cfg "db.port"
    user <- C.require cfg "db.user"
    pass <- C.require cfg "db.pass"
    name <- C.require cfg "db.name"
    return $ DBInfo host port user pass name
