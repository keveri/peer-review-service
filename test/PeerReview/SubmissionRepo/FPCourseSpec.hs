{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourseSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.Hspec.Wai

import           PeerReview.Config
import qualified PeerReview.SubmissionRepo.FPCourse              as FPCourse
import qualified PeerReview.SubmissionRepo.FPCourseMockAPIClient as FPCourseMockAPIClient
import           PeerReview.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe ".listAll" $
    it "parses all submissions from test data" $ do
      r <- repo
      allRepos <- srAll r
      length allRepos `shouldBe` 6
  describe ".forTask" $
    it "filters submissions by task name" $ do
      r <- repo
      let taskID = "SimpleTypeDrivenExercise"
      subsByTask <- srFindByTaskId r taskID
      length subsByTask `shouldBe` 2
  describe ".forUser" $
    it "filters submissions by user email" $ do
      r <- repo
      let userID = "mikko@cc.jyu.fi"
      subsByUser <- srFindByUserId r userID
      length subsByUser `shouldBe` 3
  describe ".byId" $
    it "parses submission data correctly" $ do
      r <- repo
      let subId = "sha1"
      submission <- srFindById r subId
      sId <$> submission `shouldBe` Just "sha1"
      sUid <$> submission `shouldBe` Just "mikko@cc.jyu.fi"
      sTid <$> submission `shouldBe` Just "SimpleTypeDrivenExercise"
      sContent <$> submission `shouldBe` Just (Just "Submission Content")

repo :: IO SubmissionRepo
repo = do
    let client = FPCourseMockAPIClient.client
    FPCourse.repoWithClient client <$> readSubmissionRepoConfig "test/fixtures/config/submission_repo.json"
