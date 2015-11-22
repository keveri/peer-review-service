{-# LANGUAGE OverloadedStrings #-}
module Main where

import           PeerReview.DataSource.FPCourse as FPCourse
import           PeerReview.Server
import           PeerReview.Types

main :: IO ()
main = do
    let env = Env FPCourse.dataSource
    runServer "app.cfg" env
