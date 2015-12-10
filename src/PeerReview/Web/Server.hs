{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Web.Server
    ( runServer
    , service -- Exposed for testing.
    ) where

import           Network.Wai.Middleware.RequestLogger
import           Web.Spock.Safe

import           PeerReview.Types
import           PeerReview.Web.Routes as Routes
import           PeerReview.Web.Types

-- Run the spock app using given configuration file.
runServer :: AppConfig -> Env -> IO ()
runServer conf env = do
    let port     = acPort conf
        state    = AppState env
        spockCfg = defaultSpockCfg Nothing PCNoDatabase state
    runSpock port $ spock spockCfg service


-- Middlewares for the application.
appMiddleware :: WebApp ()
appMiddleware = middleware logStdout

-- Combine different routes for the api.
apiRoutes :: WebApp ()
apiRoutes = do
    get root $ text "doc or client"
    subcomponent "api" Routes.peerReview

-- Join middlewares and API to spock app.
service :: WebApp ()
service = appMiddleware >> apiRoutes
