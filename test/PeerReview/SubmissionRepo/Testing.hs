{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.Testing
    ( repo
    ) where

import           PeerReview.Types

repo :: SubmissionRepo
repo = SubmissionRepo byId forTask forUser getAll


testSubs :: [Submission]
testSubs =
    [ Submission "1" "user1" "task1" Nothing
    , Submission "2" "user2" "task1" Nothing
    , Submission "3" "user2" "task2" Nothing
    ]

byId :: SubmissionID -> IO (Maybe Submission)
byId _ = return $ Just (Submission "1" "2" "3" (Just "wat"))

forTask :: TaskID -> IO [Submission]
forTask _ = return []

forUser :: UserID -> IO [Submission]
forUser uid = return $ filter (\ s -> uid == sUid s) testSubs

getAll :: IO [Submission]
getAll = return testSubs
