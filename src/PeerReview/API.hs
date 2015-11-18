{-# LANGUAGE OverloadedStrings #-}
module PeerReview.API
    ( new
    , create
    , list
    , doc
    ) where

import           Web.Spock.Shared

import           PeerReview.Core
import           PeerReview.Types

-- Give a task to review if possible.
new :: Action ctx a
new = do
    email <- param' "email"
    json $ findTaskToReview email

-- Create peer reviews.
create :: Action ctx a
create = do
    review <- jsonBody'
    json $ createReview review

-- List peer reviews for user.
list :: Action ctx a
list = do
    email <- param' "email"
    json $ listReviewsForUser email

-- Render API documentation.
doc :: Action ctx a
doc = text "API documentation."
