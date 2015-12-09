{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module PeerReview.Web.ServerSpec
    ( main
    , spec
    ) where

import           Network.Wai                       (Application)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Web.Spock.Safe                    (spock)
import           Web.Spock.Shared                  (PoolOrConn (..),
                                                    defaultSpockCfg, spockAsApp)

import           PeerReview.ReviewRepo.Postgres    as Postgres
import           PeerReview.SubmissionRepo.Testing as Testing
import           PeerReview.Types
import           PeerReview.Web.Server             (service)
import           PeerReview.Web.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = before_ (wipeDb dbInfo) $ with app $ do
  describe "GET non existing url" $
    it "responds with 404" $
      get "/non-existing-url" `shouldRespondWith` 404

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

  describe "GET reviews for user" $
    it "responds with 200" $
      get "/peer-reviews?email=test" `shouldRespondWith` 200

  describe "GET completed reviews" $
    it "responds with 200" $
      get "/peer-reviews/completed" `shouldRespondWith` 200

app :: IO Application
app = do
    rRepo <- Postgres.repo dbInfo
    let state    = AppState $ Env Testing.repo rRepo
        spockCfg = defaultSpockCfg Nothing PCNoDatabase state
    spockAsApp $ spock spockCfg service

dbInfo :: DBInfo
dbInfo = DBInfo "localhost" 5432 "test" "test" "peer_review_test"
