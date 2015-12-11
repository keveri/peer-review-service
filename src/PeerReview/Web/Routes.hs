{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Web.Routes
    ( peerReview
    ) where

import           Web.Spock.Safe

import           PeerReview.Web.API   as API
import           PeerReview.Web.Types

peerReview :: WebApp ()
peerReview =
    subcomponent "peer-reviews" $ do
        get  "/"      API.list
        get  var      API.find
        put  var      API.update
        post "create" API.create
