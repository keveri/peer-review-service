{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Core
    ( findTaskToReview
    , listReviewsForUser
    , createReview
    ) where

import           PeerReview.Types

-- Find reviewable task for given user.
findTaskToReview :: Email -> Task
findTaskToReview _ = Task "wat" 1

-- List all reviews done by given user.
listReviewsForUser :: Email -> [PeerReview]
listReviewsForUser _ =
    [ PeerReview (Task "1" 1) "" 1 ""
    ]

-- Create new PeerReview.
createReview :: PeerReview -> PeerReview
createReview pr = pr
