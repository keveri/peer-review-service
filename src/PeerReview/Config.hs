{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Config
    ( readConfig
    ) where

import qualified Data.Configurator as C

import           PeerReview.Types

-- Read config file.
readConfig :: FilePath -> IO AppConfig
readConfig cfgFile = do
    cfg  <- C.load [C.Required cfgFile]
    port <- C.require cfg "port"
    return $ AppConfig port
