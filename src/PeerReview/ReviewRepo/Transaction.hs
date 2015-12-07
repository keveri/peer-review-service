{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module PeerReview.ReviewRepo.Transaction
    ( saveReview
    , findReviewsByUserId
    ) where

import           Control.Monad                    (void)
import           Data.Pool                        (Pool, withResource)
import           Database.PostgreSQL.Simple       (Connection, Only (..),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           PeerReview.Types

-- Save a Peer Review to db.
saveReview :: Pool Connection -> PeerReview -> IO ()
saveReview pool pr = withResource pool (\ conn ->
    void $ execute conn [sql|
        INSERT INTO peer_reviews (submission_id, task_id,comment,score,reviewer_id,status)
        VALUES (?,?,?,?,?,?)
    |] pr)

-- Find all reviews for given User ID.
findReviewsByUserId :: Pool Connection -> UserID -> IO [PeerReview]
findReviewsByUserId pool uid = withResource pool (\ conn ->
    query conn [sql|
        SELECT submission_id, task_id, comment, score, reviewer_id, status
        FROM peer_reviews
        WHERE reviewer_id = ?
    |] $ Only uid)