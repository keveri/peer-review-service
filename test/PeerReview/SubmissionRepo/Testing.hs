{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.Testing
    ( repo
    ) where

import           PeerReview.Types

repo :: SubmissionRepo
repo = SubmissionRepo byId forTask forUser getAll


byId :: SubmissionID -> IO Submission
byId _ = return $ Submission "1" "2" "3" (Just "wat")

forTask :: TaskID -> IO [Submission]
forTask _ = return []

forUser :: UserID -> IO [Submission]
forUser _ = return []

getAll :: IO [Submission]
getAll = return []
