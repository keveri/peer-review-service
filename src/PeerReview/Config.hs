{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Config
    ( readConfig
    ) where

import qualified Data.Configurator       as C
import           Data.Configurator.Types

import           PeerReview.Types

-- Read config file.
readConfig :: FilePath -> IO AppConfig
readConfig cfgFile = do
    cfg      <- C.load [C.Required cfgFile]
    port     <- C.require cfg "port"
    database <- parseDB cfg
    return $ AppConfig port database


-- Parse database related information.
parseDB :: Config -> IO DBInfo
parseDB cfg = do
    host <- C.require cfg "db.host"
    port <- C.require cfg "db.port"
    user <- C.require cfg "db.user"
    pass <- C.require cfg "db.pass"
    name <- C.require cfg "db.name"
    return $ DBInfo host port user pass name
