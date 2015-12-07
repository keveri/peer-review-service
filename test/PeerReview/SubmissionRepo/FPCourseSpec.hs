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
  describe ".listAll" $ do
    it "parses all submissions from test data" $ do
      r <- repo
      allRepos <- srAll r
      length allRepos `shouldBe` 6
  describe ".forTask" $ do
    it "filters submissions by task name" $ do
      r <- repo
      let taskID = "SimpleTypeDrivenExercise"
      subsByTask <- srFindByTaskId r $ taskID
      length subsByTask `shouldBe` 2

repo :: IO SubmissionRepo
repo = do
    let client = FPCourseMockAPIClient.client
    FPCourse.repoWithClient client <$> readSubmissionRepoConfig "test/fixtures/config/submission_repo.json"
