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
repo :: APIClient -> SubmissionRepoConfig -> SubmissionRepo
repo client cfg =
    SubmissionRepo (byId cfg client) (forTask cfg client) (forUser cfg client) (listAll cfg client)

byId :: SubmissionRepoConfig -> APIClient ->SubmissionID -> IO Submission
byId _ _ _ = return $ Submission "1" "2" "3" (Just "wat")

forTask :: SubmissionRepoConfig -> APIClient -> TaskID -> IO [Submission]
forTask cfg client taskId = do
    allSubmissions <- getAllSubmissions cfg client
    let subsForTask = liftM (V.filter (\s -> sTid s == taskId)) allSubmissions
    case subsForTask of
        Just submissions -> return $ V.toList submissions
        _                -> return []

forUser :: SubmissionRepoConfig -> APIClient -> UserID -> IO [Submission]
forUser _ _ _ = return []

listAll :: SubmissionRepoConfig -> APIClient -> IO [Submission]
listAll cfg client = do
    allSubmissions <- getAllSubmissions cfg client
    case allSubmissions of
        Just submissions -> return $ V.toList submissions
        _                -> return []

getAllSubmissions :: SubmissionRepoConfig -> APIClient -> IO (Maybe (Vector Submission))
getAllSubmissions cfg client = do
    jsonResp <- acGetResource client $ unpack (cfg M.! (pack "listAllUrl"))
    let fpExercises = decode jsonResp :: Maybe (Vector (Vector Text))
        submissions = fmap exToSubmission <$> fpExercises
    return submissions

-- submission json structure:
-- [email, exName, submissionID]
exToSubmission :: Vector Text -> Submission
exToSubmission v =
    Submission (v V.! 2) (v V.! 0) (v V.! 1) Nothing
