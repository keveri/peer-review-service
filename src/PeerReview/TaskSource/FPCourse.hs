{-# LANGUAGE OverloadedStrings #-}
module PeerReview.TaskSource.FPCourse
    ( taskSource
    ) where

import           PeerReview.Types

-- Create task source using endpoint configuration.
-- Config is a map containing required endpoints for fetching data.
taskSource :: TaskSourceConfig -> TaskSource
taskSource _ = TaskSource byId forUser listAll


-- TODO: Implementation of task finding.
byId :: TaskID -> IO Task
byId _ = return $ Task "1" "2" (Just "wat")

-- TODO: Implementation of task listing for user.
forUser :: UserID -> IO [Task]
forUser _ = return []

-- TODO: Implementation of listing all tasks.
listAll :: IO [Task]
listAll = return []
