{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Util
    ( reviewFromSub
    ) where

import           PeerReview.Types

reviewFromSub :: UserID -> Submission -> PeerReview
reviewFromSub uid s =
    PeerReview (sId s)
               (sTid s)
               ""
               0
               uid
               Waiting
