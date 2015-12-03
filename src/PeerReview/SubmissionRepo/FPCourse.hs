{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourse
    ( repo
    ) where

import           PeerReview.Types

-- Create submission repo using endpoint configuration.
-- Config is a map containing required endpoints for fetching data.
repo :: SubmissionRepoConfig -> SubmissionRepo
repo _ = SubmissionRepo byId forUser listAll


byId :: SubmissionID -> IO Submission
byId _ = return $ Submission "1" "2" "3" (Just "wat")

forUser :: UserID -> IO [Submission]
forUser _ = return []

listAll :: IO [Submission]
listAll = return []