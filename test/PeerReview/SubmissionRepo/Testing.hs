{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.Testing
    ( repo
    ) where

import           PeerReview.Types

repo :: SubmissionRepo
repo = SubmissionRepo byId forTask forUser getAll


testSubs :: [Submission]
testSubs =
    [ Submission "1" "user1" "task1"
    , Submission "2" "user2" "task1"
    , Submission "3" "user2" "task2"
    ]

byId :: SubmissionID -> IO (Maybe SubmissionDetails)
byId _ = return $ Just (SubmissionDetails "1" "2" ["3"] "wat")

forTask :: TaskID -> IO [Submission]
forTask _ = return []

forUser :: UserID -> IO [Submission]
forUser uid = return $ filter (\ s -> uid == sSender s) testSubs

getAll :: IO [Submission]
getAll = return testSubs
