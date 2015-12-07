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
spec =
  describe ".listAll" $
    it "parses all submissions from test data" $ do
      allRepos <- repo >>= srAll
      length allRepos `shouldBe` 6

repo :: IO SubmissionRepo
repo = do
    let client = FPCourseMockAPIClient.client
    FPCourse.repoWithClient client <$> readSubmissionRepoConfig "test/fixtures/config/submission_repo.json"
