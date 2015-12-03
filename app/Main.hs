{-# LANGUAGE OverloadedStrings #-}
module Main where

import           PeerReview.Config
import           PeerReview.Database
import           PeerReview.Server
import qualified PeerReview.SubmissionRepo.FPCourse as FPCourse
import           PeerReview.Types

main :: IO ()
main = do
    appConf <- readAppConfig "app.cfg"
    pool    <- mkPoolAndInitDb $ acDB appConf
    repo    <- FPCourse.repo <$> readSubmissionRepoConfig "submission_repo.json"
    let env = Env repo pool
    runServer appConf env
