{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Web.Routes
    ( peerReview
    ) where

import           Web.Spock.Safe

import           PeerReview.Web.API   as API
import           PeerReview.Web.Types

peerReview :: WebApp ()
peerReview = do
    get   "/peer-reviews"           API.list
    get  ("/peer-reviews" <//> var) API.find
    put  ("/peer-reviews" <//> var) API.update
    post  "/peer-reviews/create"    API.create
