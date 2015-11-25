{-# LANGUAGE OverloadedStrings #-}
module PeerReview.Server
    ( runServer
    , service -- Exposed for testing.
    ) where

import           Network.Wai.Middleware.RequestLogger
import           Web.Spock.Safe

import           PeerReview.API                       as API
import           PeerReview.Config
import           PeerReview.DBConnection
import           PeerReview.Types

-- Run the spock app using given configuration file.
runServer :: FilePath -> Env -> IO ()
runServer fp env = do
    conf <- readConfig fp
    let port     = acPort conf
        state    = AppState env
        conn     = PCConn $ mkConnBuilder $ acDB conf
        spockCfg = defaultSpockCfg Nothing conn state
    runSpock port $ spock spockCfg service


-- Middlewares for the application.
appMiddleware :: WebApp ()
appMiddleware = middleware logStdout

-- Routes for the API.
apiRoutes :: WebApp ()
apiRoutes = do
    get root                                         API.doc
    get "/peer-reviews"                              API.list
    put ("/peer-reviews/" <//> ":id")                API.review
    get "/peer-reviews/new"                          API.new
    get "/peer-reviews/completed"                    API.completed
    put ("/peer-reviews/" <//> ":id" <//> "/accept") API.accept

-- Join middlewares and API to spock app.
service :: WebApp ()
service = appMiddleware >> apiRoutes
