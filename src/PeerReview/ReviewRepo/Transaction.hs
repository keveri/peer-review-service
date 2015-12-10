{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module PeerReview.ReviewRepo.Transaction
    ( saveReview
    , findReviewsByUserId
    , findById
    , update
    ) where

import           Data.Pool                        (Pool, withResource)
import           Database.PostgreSQL.Simple       (Connection, Only (..),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           PeerReview.Types

-- Save a Peer Review to db.
saveReview :: Pool Connection -> PeerReview -> IO PeerReviewID
saveReview pool pr = withResource pool (\ conn ->
    execute conn [sql|
        INSERT INTO peer_reviews (submission_id, task_id, submission_content, comment, score, reviewer_id, status)
        VALUES (?,?,?,?,?,?,?)
    |] pr)

-- Find all reviews for given User ID.
findReviewsByUserId :: Pool Connection -> UserID -> IO [(PeerReviewID,PeerReview)]
findReviewsByUserId pool uid = withResource pool (\ conn -> do
    reviews <- query conn [sql|
        SELECT id, submission_id, task_id, submission_content, comment, score, reviewer_id, status
        FROM peer_reviews
        WHERE reviewer_id = ?
    |] $ Only uid
    return $ fmap rowToPair reviews
    )

-- Find peer review by id.
findById :: Pool Connection -> PeerReviewID -> IO (Maybe (PeerReviewID,PeerReview))
findById pool rid = withResource pool (\ conn -> do
    review <- query conn [sql|
        SELECT id, submission_id, task_id, submission_content, comment, score, reviewer_id, status
        FROM peer_reviews
        WHERE id = ?
    |] $ Only rid
    case review of
        [r] -> return $ Just $ rowToPair r
        _   -> return Nothing
    )

-- Update review score and comment.
update :: Pool Connection -> PeerReviewID -> (Comment, Score) -> IO Bool
update pool rid (c,s) = withResource pool (\ conn -> do
    modified <- execute conn [sql|
        UPDATE peer_reviews
        SET comment = ?, score = ?, status = ?
        WHERE id = ?
    |] (c, s, Reviewed, rid)
    case modified of
        1 -> return True
        _ -> return False
    )



-- Turn a row from DB to a pair of id and review.
rowToPair :: ( PeerReviewID
             , SubmissionID
             , TaskID
             , Content
             , Comment
             , Score
             , UserID
             , ReviewStatus )
             -> (PeerReviewID, PeerReview)
rowToPair (prid, sid, tid, sc, c, score, rid, status) =
    (prid, PeerReview sid tid sc c score rid status)
