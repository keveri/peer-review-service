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
findTaskToReview :: Env -> UserID -> IO PeerReview
findTaskToReview _ _ = return $ PeerReview "1" "wat" 1 "2" Waiting

-- List all reviews done by given user.
listReviewsForUser :: UserID -> IO [PeerReview]
listReviewsForUser _ = return []

-- Create new PeerReview.
createReview :: PeerReview -> IO PeerReview
createReview = return

-- Mark review as accepted.
acceptReview :: PeerReview -> IO PeerReview
acceptReview = return

-- List of completed reviews.
completedReviews :: IO [PeerReview]
completedReviews = return []
