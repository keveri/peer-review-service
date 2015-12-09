module PeerReview.Web.Types where

import           Web.Spock.Safe

import           PeerReview.Types

data AppState = AppState
    { asEnv :: Env
    }

type SessionVal = Maybe SessionId
type WebApp ctx = SpockCtxM ctx () SessionVal AppState ()
type Action ctx a = SpockActionCtx ctx () SessionVal AppState a
