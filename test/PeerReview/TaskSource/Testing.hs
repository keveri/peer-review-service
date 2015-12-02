{-# LANGUAGE OverloadedStrings #-}
module PeerReview.TaskSource.Testing
    ( taskSource
    ) where

import           PeerReview.Types

taskSource :: TaskSource
taskSource = TaskSource findTaskById tasksForUser allTasks


-- Implementation of task finding.
findTaskById :: TaskID -> IO Task
findTaskById _ = return $ Task "1" "2" (Just "wat")

-- Implementation of task listing for user.
tasksForUser :: UserID -> IO [Task]
tasksForUser _ = return []

allTasks :: IO [Task]
allTasks = return []
