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
findTaskToReview :: Env -> UserID -> IO (Maybe (PeerReviewID,PeerReview))
findTaskToReview env uid = do
    mSubmission <- findSubmissionToReview env uid
    case reviewFromSub uid <$> mSubmission of
        Nothing -> return Nothing
        Just r  -> do
            rid <- rrSave (eReviewRepo env) r
            return $ Just (rid,r)

listReviewsForUser :: Env -> UserID -> IO [(PeerReviewID,PeerReview)]
listReviewsForUser = rrFindByUserId . eReviewRepo

updateReview :: Env -> PeerReviewID -> (Text, Int) -> IO Bool
updateReview = rrUpdate . eReviewRepo

findReview :: Env -> PeerReviewID -> IO (Maybe (PeerReviewID,PeerReview))
findReview = rrFindById . eReviewRepo
