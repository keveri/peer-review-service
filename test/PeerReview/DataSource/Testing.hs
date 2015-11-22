{-# LANGUAGE OverloadedStrings #-}
module PeerReview.DataSource.Testing
    ( dataSource
    ) where

import           PeerReview.Types

dataSource :: DataSource
dataSource = DataSource findTaskById tasksForUser


-- Implementation of task finding.
findTaskById :: TaskID -> IO ReviewTask
findTaskById _ = return $ ReviewTask "wat" "1" "2"

-- Implementation of task listing for user.
tasksForUser :: UserID -> IO [ReviewTask]
tasksForUser _ = return []
