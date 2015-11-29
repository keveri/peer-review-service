{-# LANGUAGE OverloadedStrings #-}
module PeerReview.API
    ( new
    , review
    , list
    , doc
    , completed
    , accept
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Web.Spock.Shared

import           PeerReview.Core
import           PeerReview.Types

-- Give a task to review if possible.
new :: Action ctx a
new = do
    email <- param' "email"
    e <- asEnv <$> getState
    json =<< liftIO (findTaskToReview e email)

-- Create peer reviews.
review :: Action ctx a
review = do
    r <- jsonBody'
    json =<< liftIO (createReview r)

-- List peer reviews for user.
list :: Action ctx a
list = do
    email <- param' "email"
    e <- asEnv <$> getState
    json =<< liftIO (listReviewsForUser e email)

-- List completed reviews.
completed :: Action ctx a
completed = json =<< liftIO completedReviews

-- Mark review as accepted.
accept :: Action ctx a
accept = do
    r <- jsonBody'
    json =<< liftIO (acceptReview r)

-- Render API documentation.
doc :: Action ctx a
doc = text "API documentation."
