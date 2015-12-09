{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Web.Types where

import           Data.Aeson
import           Web.Spock.Safe

import           PeerReview.Types

data AppState = AppState
    { asEnv :: Env
    }

type SessionVal = Maybe SessionId
type WebApp ctx = SpockCtxM ctx () SessionVal AppState ()
type Action ctx a = SpockActionCtx ctx () SessionVal AppState a

data CreateReviewBody = CreateReviewBody
    { crbUid :: UserID
    }

instance FromJSON CreateReviewBody where
    parseJSON (Object o) =
        CreateReviewBody <$> o .: "userID"
    parseJSON _ = mempty
