{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.Testing
    ( repo
    ) where

import           PeerReview.Types

repo :: SubmissionRepo
repo = SubmissionRepo findById findForUser getAll


findById :: TaskID -> IO Submission
findById _ = return $ Submission "1" "2" "3" (Just "wat")

findForUser :: UserID -> IO [Submission]
findForUser _ = return []

getAll :: IO [Submission]
getAll = return []
