{-# LANGUAGE OverloadedStrings #-}
module PeerReview.ServerSpec
    ( main
    , spec
    ) where

import           Data.Aeson
import           Test.Hspec
import           Test.Hspec.Wai
import           Web.Spock.Safe                    (spock)
import           Web.Spock.Shared

import           PeerReview.Database
import           PeerReview.Server                 (service)
import           PeerReview.SubmissionRepo.Testing as Testing
import           PeerReview.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200
  describe "GET new review" $
    it "responds with error when no submission can be reviewed" $ do
      let jsonError = "{\"code\":1,\"message\":\"No submissions to review.\"}"
      get "/peer-reviews/new?email=test" `shouldRespondWith` jsonError

app = do
    pool <- mkPoolAndInitDb dbInfo
    let state    = AppState $ Env Testing.repo pool
        spockCfg = defaultSpockCfg Nothing PCNoDatabase state
    spockAsApp $ spock spockCfg service

dbInfo :: DBInfo
dbInfo = DBInfo "localhost" 5432 "test" "test" "peer_review_test"
