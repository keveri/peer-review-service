{-# LANGUAGE OverloadedStrings #-}
module Main where

import           PeerReview.Config
import           PeerReview.ReviewRepo.Postgres     as Postgres
import qualified PeerReview.SubmissionRepo.FPCourse as FPCourse
import           PeerReview.Types
import           PeerReview.Web.Server

main :: IO ()
main = do
    appConf <- readAppConfig "app.cfg"
    rRepo   <- Postgres.repo $ acDB appConf
    sRepo   <- FPCourse.repo <$> readSubmissionRepoConfig "submission_repo.json"
    let env = Env sRepo rRepo
    runServer appConf env
