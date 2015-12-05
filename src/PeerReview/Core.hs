{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Core
    ( findTaskToReview
    , listReviewsForUser
    , createReview
    , acceptReview
    , completedReviews
    ) where

import           PeerReview.ReviewFinder
import           PeerReview.Transaction
import           PeerReview.Types
import           PeerReview.Util

-- Find reviewable task for given user.
-- TODO: Use Either on return type to handle error cases.
findTaskToReview :: Env -> UserID -> IO PeerReview
findTaskToReview env uid = do
    mSubmission <- findSubmissionToReview env uid
    let err   = PeerReview "1" "1" "error" 1 uid Waiting
        mReview = reviewFromSub uid <$> mSubmission
    case mReview of
        Nothing -> return err
        Just r  -> do
            saveReview (ePool env) r
            return r

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
