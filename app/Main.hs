{-# LANGUAGE OverloadedStrings #-}
module Main where

import           PeerReview.Server

main :: IO ()
main = runServer "app.cfg"
