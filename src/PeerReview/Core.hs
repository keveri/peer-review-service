{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Core
    ( findTaskToReview
    , listReviewsForUser
    , updateReview
    , findReview
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

listReviewsForUser :: Env -> UserID -> IO [PeerReview]
listReviewsForUser = rrFindByUserId . eReviewRepo

updateReview :: IO PeerReview
updateReview = return $ PeerReview "" "" "" 1 "" Reviewed

findReview :: Env -> PeerReviewID -> IO (Either ErrorMessage PeerReview)
findReview env rid = do
    let err = ErrorMessage "Not found." 404
    mReview <- rrFindById (eReviewRepo env) rid
    case mReview of
        Nothing -> return $ Left err
        Just r  -> return $ Right r
