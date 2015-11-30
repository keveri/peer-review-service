{-# LANGUAGE OverloadedStrings #-}
module Main where

import           PeerReview.Config
import           PeerReview.Database
import           PeerReview.Server
import qualified PeerReview.TaskSource.FPCourse as FPCourse
import           PeerReview.Types

main :: IO ()
main = do
    appConf    <- readAppConfig "app.cfg"
    pool       <- mkPoolAndInitDb $ acDB appConf
    taskSource <- FPCourse.taskSource <$> readTaskSourceConfig "task_source.json"
    let env = Env taskSource pool
    runServer appConf env
