{-# LANGUAGE OverloadedStrings #-}
module PeerReview.DataSource.FPCourse
    ( dataSource
    ) where

import           PeerReview.Types

-- Data source for functional programming course.
dataSource :: DataSource
dataSource = DataSource findTaskById tasksForUser


-- Implementation of task finding.
findTaskById :: TaskID -> IO ReviewTask
findTaskById _ = return $ ReviewTask "wat" "1" "2"

-- Implementation of task listing for user.
tasksForUser :: UserID -> IO [ReviewTask]
tasksForUser _ = return []
