{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Core
    ( findTaskToReview
    , listReviewsForUser
    , createReview
    , acceptReview
    , completedReviews
    ) where

import           PeerReview.ReviewFinder
import           PeerReview.Types
import           PeerReview.Util

-- Find reviewable task for given user.
findTaskToReview :: Env -> UserID -> IO (Either ErrorMessage PeerReview)
findTaskToReview env uid = do
    mSubmission <- findSubmissionToReview env uid
    let err     = ErrorMessage "No submissions to review." 1
        mReview = reviewFromSub uid <$> mSubmission
    case mReview of
        Nothing -> return $ Left err
        Just r  -> do
            rrSave (eReviewRepo env) r
            return $ Right r

-- List all reviews done by given user.
listReviewsForUser :: Env -> UserID -> IO [PeerReview]
listReviewsForUser = rrFindByUserId . eReviewRepo

-- Create new PeerReview.
createReview :: PeerReview -> IO PeerReview
createReview = return

-- Mark review as accepted.
acceptReview :: PeerReview -> IO PeerReview
acceptReview = return

-- List of completed reviews.
completedReviews :: IO [PeerReview]
completedReviews = return []
