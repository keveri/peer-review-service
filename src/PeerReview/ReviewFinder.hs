-- This is an example implementation for a module implementing the logic for
-- finding a new submission to review.
module PeerReview.ReviewFinder
    ( findSubmissionToReview
    ) where

import           Data.List              (find)
import qualified Data.Set               as Set

import           PeerReview.Transaction
import           PeerReview.Types


-- A simple version of a function choosing next submnission to review.
findSubmissionToReview :: Env -> UserID -> IO (Maybe Submission)
findSubmissionToReview (Env sr pool) uid = do
    userSubs    <- srFindByUserId sr uid
    userReviews <- findReviewsByUserId pool uid
    allSubs     <- srAll sr
    return $ pickOne allSubs $ candidates userSubs userReviews


-- Already reviewed tasks are not candidates for the next review.
candidates :: [Submission] -> [PeerReview] -> [TaskID]
candidates ss prs =
    let submitted = Set.fromList $ map sTid ss
        reviewed  = Set.fromList $ map prTaskId prs
    in Set.toList $ submitted Set.\\ reviewed

-- Just try to use the first task id.
pickOne :: [Submission] -> [TaskID] -> Maybe Submission
pickOne _ []     = Nothing
pickOne ss (x:_) = find ((==) x . sTid) ss
