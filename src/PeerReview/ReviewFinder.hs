-- This is an example implementation for a module implementing the logic for
-- finding a new submission to review.
module PeerReview.ReviewFinder
    ( findSubmissionToReview
    ) where

import           Data.List              (find)
import qualified Data.Set               as Set

import           PeerReview.Types


-- A simple version of a function choosing next submnission to review.
findSubmissionToReview :: Env -> UserID -> IO (Maybe (Submission,SubmissionDetails))
findSubmissionToReview (Env sr rr) uid = do
    userSubs    <- srFindByUserId sr uid
    userReviews <- rrFindByUserId rr uid
    allSubs     <- srAll sr
    let sub = pickOne allSubs $ candidates userSubs userReviews
    case sub of
        Nothing -> return Nothing
        Just s  -> do
            mDetails <- srFindById sr $ sId s
            case mDetails of
                Nothing -> return Nothing
                Just d  -> return $ Just (s,d)


-- Already reviewed tasks are not candidates for the next review.
candidates :: [Submission] -> [(PeerReviewID,PeerReview)] -> [TaskID]
candidates ss prs =
    let submitted = Set.fromList $ map sTid ss
        reviewed  = Set.fromList $ map (prTaskId . snd) prs
    in Set.toList $ submitted Set.\\ reviewed

-- Just try to use the first task id.
pickOne :: [Submission] -> [TaskID] -> Maybe Submission
pickOne _ []     = Nothing
pickOne ss (x:_) = find ((==) x . sTid) ss
