{-# LANGUAGE OverloadedStrings #-}
module Main where

import           PeerReview.Config
import           PeerReview.Database
import           PeerReview.DataSource.FPCourse as FPCourse
import           PeerReview.Server
import           PeerReview.Types

main :: IO ()
main = do
    conf <- readConfig "app.cfg"
    pool <- mkPoolAndInitDb $ acDB conf
    let env = Env FPCourse.dataSource pool
    runServer conf env
