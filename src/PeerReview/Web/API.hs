{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Web.API
    ( create
    , update
    , list
    , find
    ) where

import           Control.Arrow             ((&&&))
import           Control.Monad             (unless)
import           Control.Monad.IO.Class    (liftIO)
import           Network.HTTP.Types.Status (status404)
import           Web.Spock.Shared

import           PeerReview.Core
import           PeerReview.Types          (PeerReview (..), PeerReviewID)
import           PeerReview.Web.Types

create :: Action ctx a
create = do
    email <- crbUid <$> jsonBody'
    e <- asEnv <$> getState
    eReview <- liftIO (findTaskToReview e email)
    either json json (reviewResponse <$> eReview)

update :: Int -> Action ctx a
update rid = do
    env        <- asEnv <$> getState
    reviewData <- (urbComment &&& urbScore) <$> jsonBody'
    success    <- liftIO (updateReview env (fromIntegral rid) reviewData)
    unless success (setStatus status404)
    json success

-- TODO: different kind of listings.
list :: Action ctx a
list = do
    let email = "user1"
    e <- asEnv <$> getState
    json =<< liftIO (fmap reviewResponse <$> listReviewsForUser e email)

find :: Int -> Action ctx a
find rid = do
    env     <- asEnv <$> getState
    eResult <- liftIO (findReview env (fromIntegral rid))
    either json json (reviewResponse <$> eResult)


-- Turn Review data into API format.
reviewResponse :: (PeerReviewID,PeerReview) -> ReviewResponse
reviewResponse (rid, PeerReview sid tid c score revid status) =
    ReviewResponse rid sid tid c score revid status
