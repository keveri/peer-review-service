{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourseMockAPIClient
    ( client
    ) where

import qualified Data.ByteString.Lazy as B

import           PeerReview.Types

client :: APIClient
client = APIClient getResource

getResource :: String -> IO B.ByteString
getResource "testURLById/sha1" = B.readFile "test/fixtures/FPCourse/sha1.json"
getResource _ = B.readFile "test/fixtures/FPCourse/all.json"
