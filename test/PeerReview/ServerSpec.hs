{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module PeerReview.ServerSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Web.Spock.Safe                    (spock)
import           Web.Spock.Shared                  (PoolOrConn (..),
                                                    defaultSpockCfg, spockAsApp)

import           PeerReview.ReviewRepo.Postgres    as Postgres
import           PeerReview.Server                 (service)
import           PeerReview.SubmissionRepo.Testing as Testing
import           PeerReview.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = before_ (wipeDb dbInfo) $ with app $ do
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200
  describe "GET new review" $ do
    context "when no submission can be reviewed" $
      it "responds with an error message" $ do
        let jsonError = [json|
              { code: 1
              , message: "No submissions to review."
              }|]
        get "/peer-reviews/new?email=test" `shouldRespondWith` jsonError
    context "when submission can be reviewed" $
      it "responds with an peer review" $ do
        let jsonReview = [json|
              { status: "waiting"
              , submissionId: "1"
              , score: 0
              , reviewerId: "user1"
              , comment: ""
              }|]
        get "/peer-reviews/new?email=user1" `shouldRespondWith` jsonReview

app = do
    rRepo <- Postgres.repo dbInfo
    let state    = AppState $ Env Testing.repo rRepo
        spockCfg = defaultSpockCfg Nothing PCNoDatabase state
    spockAsApp $ spock spockCfg service

dbInfo :: DBInfo
dbInfo = DBInfo "localhost" 5432 "test" "test" "peer_review_test"
