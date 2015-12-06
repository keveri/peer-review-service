{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourse
    ( repo
    ) where

import           PeerReview.Types
import           Data.Aeson
import           Data.Vector                     as V
import           Data.Map                        as M
import           Data.Text
import           Control.Monad
import           PeerReview.SubmissionRepo.FPCourseAPIClient

-- Create submission repo using endpoint configuration.
-- Config is a map containing required endpoints for fetching data.
repo :: SubmissionRepoConfig -> SubmissionRepo
repo cfg = SubmissionRepo (byId cfg) (forTask cfg) (forUser cfg) (listAll cfg)

byId :: SubmissionRepoConfig -> SubmissionID -> IO Submission
byId _ _ = return $ Submission "1" "2" "3" (Just "wat")

forTask :: SubmissionRepoConfig -> TaskID -> IO [Submission]
forTask cfg taskId = do
    allSubmissions <- getAllSubmissions . unpack $ cfg M.! pack "listAllUrl"
    let subsForTask = liftM (V.filter (\s -> sTid s == taskId)) allSubmissions
    case subsForTask of
        Just submissions -> return $ V.toList submissions
        _                -> return []

forUser :: SubmissionRepoConfig -> UserID -> IO [Submission]
forUser _ _ = return []

listAll :: SubmissionRepoConfig -> IO [Submission]
listAll cfg = do
    allSubmissions <- getAllSubmissions . unpack $ cfg M.! pack "listAllUrl"
    case allSubmissions of
        Just submissions -> return $ V.toList submissions
        _                -> return []

getAllSubmissions :: String -> IO (Maybe (Vector Submission))
getAllSubmissions url = do
    jsonResp <- getJSONResource url
    let fpExercises = decode jsonResp :: Maybe (Vector (Vector Text))
        submissions = fmap exToSubmission <$> fpExercises
    return submissions

-- submission json structure:
-- [email, exName, submissionID]
exToSubmission :: Vector Text -> Submission
exToSubmission v =
    Submission (v V.! 2) (v V.! 0) (v V.! 1) Nothing
