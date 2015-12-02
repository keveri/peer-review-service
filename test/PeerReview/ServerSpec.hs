{-# LANGUAGE OverloadedStrings #-}
module PeerReview.ServerSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Web.Spock.Safe                (spock)
import           Web.Spock.Shared

import           PeerReview.Database
import           PeerReview.Server             (service)
import           PeerReview.TaskSource.Testing as Testing
import           PeerReview.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

app = do
    pool <- mkPoolAndInitDb dbInfo
    let state    = AppState $ Env Testing.taskSource pool
        spockCfg = defaultSpockCfg Nothing PCNoDatabase state
    spockAsApp $ spock spockCfg service

dbInfo :: DBInfo
dbInfo = DBInfo "localhost" 5432 "test" "test" "peer_review_test"
