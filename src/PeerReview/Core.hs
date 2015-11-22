{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Core
    ( findTaskToReview
    , listReviewsForUser
    , createReview
    , acceptReview
    , completedReviews
    ) where

import           PeerReview.Types

-- Find reviewable task for given user.
findTaskToReview :: UserID -> ReviewTask
findTaskToReview _ = ReviewTask "wat" "1" "2"

-- List all reviews done by given user.
listReviewsForUser :: UserID -> [PeerReview]
listReviewsForUser _ = []

-- Create new PeerReview.
createReview :: PeerReview -> PeerReview
createReview pr = pr

-- Mark review as accepted.
acceptReview :: PeerReview -> PeerReview
acceptReview pr = pr

-- List of completed reviews.
completedReviews :: [PeerReview]
completedReviews = []
