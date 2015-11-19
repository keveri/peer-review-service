{-# LANGUAGE OverloadedStrings #-}
module PeerReview.API
    ( new
    , review
    , list
    , doc
    , completed
    , accept
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
review :: Action ctx a
review = do
    r <- jsonBody'
    json $ createReview r

-- List peer reviews for user.
list :: Action ctx a
list = do
    email <- param' "email"
    json $ listReviewsForUser email

-- List completed reviews.
completed :: Action ctx a
completed = json completedReviews

-- Mark review as accepted.
accept :: Action ctx a
accept = do
    r <- jsonBody'
    json $ acceptReview r

-- Render API documentation.
doc :: Action ctx a
doc = text "API documentation."
