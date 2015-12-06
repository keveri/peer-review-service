{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourseMockAPIClient
    (
    client
    ) where

import           PeerReview.Types
import qualified Data.ByteString.Lazy as B

client :: APIClient
client = APIClient getResource

getResource :: String -> IO B.ByteString
getResource url =
  B.readFile "test/fixtures/FPCourse/all.json"
