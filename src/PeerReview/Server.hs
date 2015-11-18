{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Server
    ( runServer
    , service -- Exposed for testing.
    ) where

import           Network.Wai.Middleware.RequestLogger
import           Web.Spock.Safe

import           PeerReview.API                       as API
import           PeerReview.Config
import           PeerReview.Types

-- Run the spock app using given configuration file.
runServer :: FilePath -> IO ()
runServer fp = do
    conf <- readConfig fp
    let port = acPort conf
        state = AppState 1
        spockCfg = defaultSpockCfg Nothing PCNoDatabase state
    runSpock port $ spock spockCfg service


-- Middlewares for the application.
appMiddleware :: App ()
appMiddleware = middleware logStdout

-- Routes for the API.
apiRoutes :: App ()
apiRoutes = do
    get  root                API.doc
    get  "/peer-reviews"     API.list
    get  "/peer-reviews/new" API.new
    post "/peer-reviews"     API.create

-- Join middlewares and API to spock app.
service :: App ()
service = appMiddleware >> apiRoutes
