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
import           PeerReview.Web.Types

create :: Action ctx a
create = do
    email <- crbUid <$> jsonBody'
    e <- asEnv <$> getState
    eReview <- liftIO (findTaskToReview e email)
    either json json eReview

update :: Int -> Action ctx a
update _ = json =<< liftIO updateReview

-- TODO: different kind of listings.
list :: Action ctx a
list = do
    let email = "user1"
    e <- asEnv <$> getState
    json =<< liftIO (listReviewsForUser e email)

find :: Int -> Action ctx a
find _ = json =<< liftIO (findReview 0)
