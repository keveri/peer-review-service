{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Core
    ( findTaskToReview
    , listReviewsForUser
    , updateReview
    , findReview
    ) where

import           Data.Text               (Text)

import           PeerReview.ReviewFinder
import           PeerReview.Types
import           PeerReview.Util

-- Find reviewable task for given user.
findTaskToReview :: Env -> UserID -> IO (Either ErrorMessage (PeerReviewID,PeerReview))
findTaskToReview env uid = do
    mSubmission <- findSubmissionToReview env uid
    let err     = ErrorMessage "No submissions to review." 1
        mReview = reviewFromSub uid <$> mSubmission
    case mReview of
        Nothing -> return $ Left err
        Just r  -> do
            rid <- rrSave (eReviewRepo env) r
            return $ Right (rid,r)

listReviewsForUser :: Env -> UserID -> IO [(PeerReviewID,PeerReview)]
listReviewsForUser = rrFindByUserId . eReviewRepo

updateReview :: Env -> PeerReviewID -> (Text, Int) -> IO Bool
updateReview = rrUpdate . eReviewRepo

findReview :: Env -> PeerReviewID -> IO (Either ErrorMessage (PeerReviewID,PeerReview))
findReview env rid = do
    let err = ErrorMessage "Not found." 404
    mReview <- rrFindById (eReviewRepo env) rid
    case mReview of
        Nothing -> return $ Left err
        Just r  -> return $ Right r
