{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Core
    ( findTaskToReview
    , listReviews
    , updateReview
    , findReview
    ) where

import           Data.Text               (Text)

import           PeerReview.ReviewFinder
import           PeerReview.Types
import           PeerReview.Util

-- Find reviewable task for given user.
findTaskToReview :: Env -> UserID -> IO (Maybe (PeerReviewID,PeerReview))
findTaskToReview env uid = do
    mSubmissionData <- findSubmissionToReview env uid
    case reviewFromSub uid <$> mSubmissionData of
        Nothing -> return Nothing
        Just r  -> do
            rid <- rrSave (eReviewRepo env) r
            return $ Just (rid,r)

-- List reviews using given filter.
listReviews :: Env -> Maybe (ReviewFilter Text) -> IO [(PeerReviewID,PeerReview)]
listReviews e Nothing                 = rrAll (eReviewRepo e)
listReviews e (Just (ByTask tid))     = rrFindByTaskId (eReviewRepo e) tid
listReviews e (Just (ByReviewer uid)) = rrFindByUserId (eReviewRepo e) uid

updateReview :: Env -> PeerReviewID -> (Comment, Score) -> IO Bool
updateReview = rrUpdate . eReviewRepo

findReview :: Env -> PeerReviewID -> IO (Maybe (PeerReviewID,PeerReview))
findReview = rrFindById . eReviewRepo
