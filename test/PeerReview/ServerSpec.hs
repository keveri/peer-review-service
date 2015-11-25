{-# LANGUAGE OverloadedStrings #-}
module PeerReview.ServerSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Web.Spock.Safe                (spock)
import           Web.Spock.Shared

import           PeerReview.DataSource.Testing as Testing
import           PeerReview.Database
import           PeerReview.Server             (service)
import           PeerReview.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

app = do
    let state    = AppState $ Env Testing.dataSource
        conn     = PCConn $ mkConnBuilder dbInfo
        spockCfg = defaultSpockCfg Nothing conn state
    runMigrations dbInfo
    spockAsApp $ spock spockCfg service

dbInfo :: DBInfo
dbInfo = DBInfo "localhost" 5432 "user" "passwd" "test"
