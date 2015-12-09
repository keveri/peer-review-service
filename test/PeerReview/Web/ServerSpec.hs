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

  describe "POST create review" $ do
    context "when new review can be found and created" $
      it "responds with new review" $ do
        let jsonBody     = [json| {userID: "user1"} |]
            jsonResponse = [json|
                { status: "waiting"
                , submissionId: "1"
                , score: 0
                , reviewerId: "user1"
                , comment: ""
                } |]
        post "/peer-reviews/create" jsonBody `shouldRespondWith` jsonResponse
    context "when new review can't be found" $
      it "responds with an error message" $ do
        let jsonBody  = [json| {userID: "test"} |]
        let jsonError = [json|
            { code: 1
            , message: "No submissions to review."
            } |]
        post "/peer-reviews/create" jsonBody `shouldRespondWith` jsonError
    context "when JSON body is invalid" $
      it "reponds with server error" $
        post "/peer-reviews/create" "" `shouldRespondWith` 400

  describe "PUT update review" $
    it "responds with 200" $
      put "/peer-reviews/1" "" `shouldRespondWith` 200

  describe "GET review listing" $
    it "responds with 200" $
      get "/peer-reviews" `shouldRespondWith` 200

  describe "GET find review" $
    it "responds with 200" $
      get "/peer-reviews/1" `shouldRespondWith` 200

app :: IO Application
app = do
    rRepo <- Postgres.repo dbInfo
    let state    = AppState $ Env Testing.repo rRepo
        spockCfg = defaultSpockCfg Nothing PCNoDatabase state
    spockAsApp $ spock spockCfg service

dbInfo :: DBInfo
dbInfo = DBInfo "localhost" 5432 "test" "test" "peer_review_test"
