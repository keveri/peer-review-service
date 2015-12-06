{-# LANGUAGE OverloadedStrings #-}
module Main where

import           PeerReview.Config
import           PeerReview.ReviewRepo.Postgres     as Postgres
import           PeerReview.Server
import qualified PeerReview.SubmissionRepo.FPCourse as FPCourse
import           PeerReview.Types

main :: IO ()
main = do
    appConf <- readAppConfig "app.cfg"
    rRepo   <- Postgres.repo $ acDB appConf
    sRepo   <- FPCourse.repo <$> readSubmissionRepoConfig "submission_repo.json"
    let env = Env sRepo rRepo
    runServer appConf env
