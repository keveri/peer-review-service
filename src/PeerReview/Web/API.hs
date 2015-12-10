{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Web.API
    ( create
    , update
    , list
    , find
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Web.Spock.Shared

import           PeerReview.Core
import           PeerReview.Types       (PeerReview (..), PeerReviewID)
import           PeerReview.Web.Types

create :: Action ctx a
create = do
    email <- crbUid <$> jsonBody'
    e <- asEnv <$> getState
    eReview <- liftIO (findTaskToReview e email)
    either json json (reviewWithId <$> eReview)

update :: Int -> Action ctx a
update _ = json =<< liftIO (reviewWithId <$> updateReview)

-- TODO: different kind of listings.
list :: Action ctx a
list = do
    let email = "user1"
    e <- asEnv <$> getState
    json =<< liftIO (fmap reviewWithId <$> listReviewsForUser e email)

find :: Int -> Action ctx a
find rid = do
    env     <- asEnv <$> getState
    eResult <- liftIO (findReview env (fromIntegral rid))
    either json json (reviewWithId <$> eResult)


-- Turn Review data into API format.
reviewWithId :: (PeerReviewID,PeerReview) -> ReviewResponse
reviewWithId (rid, PeerReview sid tid c score revid status) =
    ReviewResponse rid sid tid c score revid status
