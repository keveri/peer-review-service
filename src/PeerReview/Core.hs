{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Core
    ( findTaskToReview
    , listReviewsForUser
    , createReview
    , acceptReview
    , completedReviews
    ) where

import           PeerReview.Transaction
import           PeerReview.Types

-- Find reviewable task for given user.
-- TODO: Correct implementation. This is just for seeing how db queries could
-- be used.
findTaskToReview :: Env -> UserID -> IO PeerReview
findTaskToReview env uid = do
    let review = PeerReview "1" "wat" 1 uid Waiting
    saveReview (ePool env) review
    return review

-- List all reviews done by given user.
listReviewsForUser :: Env -> UserID -> IO [PeerReview]
listReviewsForUser = findReviewsByUserId . ePool

-- Create new PeerReview.
createReview :: PeerReview -> IO PeerReview
createReview = return

-- Mark review as accepted.
acceptReview :: PeerReview -> IO PeerReview
acceptReview = return

-- List of completed reviews.
completedReviews :: IO [PeerReview]
completedReviews = return []
