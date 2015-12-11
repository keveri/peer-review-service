{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module PeerReview.Web.ServerSpec
    ( main
    , spec
    ) where

import           Control.Monad                     (void)
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

  describe "GET root url" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

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
                , taskId: "task1"
                , submissionContent: "wat"
                , id: 1
                } |]
        post "/api/peer-reviews/create" jsonBody `shouldRespondWith` jsonResponse
    context "when new review can't be found" $
      it "responds with an error message" $ do
        let jsonBody  = [json| {userID: "test"} |]
        post "/api/peer-reviews/create" jsonBody `shouldRespondWith` 400
    context "when JSON body is invalid" $
      it "reponds with server error" $
        post "/api/peer-reviews/create" "" `shouldRespondWith` 400

  describe "PUT update review" $ do
    context "json data is correct" $
      it "responds with 200" $ do
        let rev1     = PeerReview "1" "task1" "abc" "" 0 "user1" Waiting
            jsonBody = [json| {comment: "gj", score: 3} |]
        liftIO (saveReview rev1)
        put "/api/peer-reviews/1" jsonBody `shouldRespondWith` 200
    context "json data is incorrect" $
      it "responds with error" $
        put "/api/peer-reviews/1" "" `shouldRespondWith` 400
    context "review doesn't exist" $
      it "responds with notfound error" $ do
        let jsonBody = [json| {comment: "gj", score: 3} |]
        put "/api/peer-reviews/1" jsonBody `shouldRespondWith` 404

  describe "GET review listing" $ do
    context "when using filters" $ do
      it "can filter by task" $
        get "/api/peer-reviews?filter[task]=task1" `shouldRespondWith` 200
      it "can filter by reviewer" $
        get "/api/peer-reviews?filter[reviewer]=user1" `shouldRespondWith` 200
      it "returns error with incorrect filter" $
        get "/api/peer-reviews?filter[wat]=wat" `shouldRespondWith` 404
    context "when using no filters" $
      it "responds with full listing of reviews" $
        get "/api/peer-reviews" `shouldRespondWith` 200

  describe "GET find review" $ do
    context "when review id exists" $
      it "responds with an review JSON" $ do
        let rev1         = PeerReview "1" "task1" "abc" "ok" 3 "user1" Reviewed
            jsonResponse = [json|
                { status: "reviewed"
                , submissionId: "1"
                , score: 3
                , reviewerId: "user1"
                , comment: "ok"
                , taskId: "task1"
                , submissionContent: "abc"
                , id: 1
                } |]
        liftIO (saveReview rev1)
        get "/api/peer-reviews/1" `shouldRespondWith` jsonResponse
    context "when review id doesn't exist" $
      it "responds with an error" $
        get "/api/peer-reviews/1" `shouldRespondWith` 404

app :: IO Application
app = do
    rRepo <- Postgres.repo dbInfo
    let state    = AppState $ Env Testing.repo rRepo
        spockCfg = defaultSpockCfg Nothing PCNoDatabase state
    spockAsApp $ spock spockCfg service

-- FIXME: this isn't a nice way of creating test data."
saveReview :: PeerReview -> IO ()
saveReview pr = do
    rRepo <- Postgres.repo dbInfo
    void $ rrSave rRepo pr

dbInfo :: DBInfo
dbInfo = DBInfo "localhost" 5432 "test" "test" "peer_review_test"
