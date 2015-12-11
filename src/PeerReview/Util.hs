{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Util
    ( reviewFromSub
    ) where

import           PeerReview.Types

reviewFromSub :: UserID -> (Submission,SubmissionDetails) -> PeerReview
reviewFromSub uid (s,sd) =
    PeerReview (sId s)
               (sTid s)
               (sdContent sd)
               ""
               0
               uid
               Waiting
