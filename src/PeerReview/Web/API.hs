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
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Text                 (Text)
import           Network.HTTP.Types.Status (status400, status404)
import           Web.Spock.Shared

import           PeerReview.Core
import           PeerReview.Types          (PeerReview (..), PeerReviewID,
                                            ReviewFilter (..))
import           PeerReview.Web.Types

create :: Action ctx a
create = do
    email <- crbUid <$> jsonBody'
    e <- asEnv <$> getState
    eReview <- liftIO (findTaskToReview e email)
    maybe (setStatus status400 >> json False) json (reviewResponse <$> eReview)

update :: Int -> Action ctx a
update rid = do
    env        <- asEnv <$> getState
    reviewData <- (urbComment &&& urbScore) <$> jsonBody'
    success    <- liftIO (updateReview env (fromIntegral rid) reviewData)
    unless success (setStatus status404)
    json success

-- Find all reviews when there are no params.
-- When params are present pick correct filter or return 404.
list :: Action ctx a
list = do
    ps <- params
    e  <- asEnv <$> getState
    if length ps < 1 then
        json =<< liftIO (fmap reviewResponse <$> listReviews e Nothing) else
        case pickFilter ps of
            Nothing -> json404
            p       -> json =<< liftIO (fmap reviewResponse <$> listReviews e p)

find :: Int -> Action ctx a
find rid = do
    env     <- asEnv <$> getState
    eResult <- liftIO (findReview env (fromIntegral rid))
    maybe json404 json (reviewResponse <$> eResult)


-- Return False as JSON with status 404.
json404 :: Action ctx a
json404 = setStatus status404 >> json False

-- Turn Review data into API format.
reviewResponse :: (PeerReviewID,PeerReview) -> ReviewResponse
reviewResponse (rid, PeerReview sid tid sc c score revid status) =
    ReviewResponse rid sid tid sc c score revid status

-- All available filters for review listing.
filters :: Map Text (Text -> ReviewFilter Text)
filters = M.fromList
    [ ("filter[task]",     ByTask)
    , ("filter[reviewer]", ByReviewer)
    ]

-- Try to find a filter from params.
pickFilter :: [(Text,Text)] -> Maybe (ReviewFilter Text)
pickFilter ps =
    let f (a,b) Nothing = case M.lookup a filters of
                            Nothing -> Nothing
                            Just x  -> Just $ x b
        f _ y        = y
    in foldr f Nothing ps
