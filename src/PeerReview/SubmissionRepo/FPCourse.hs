{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourse
    ( repo
    , repoWithClient
    ) where

import           Data.Aeson
import           Data.Map                                    as M
import           Data.Text
import           Data.Vector                                 as V

import qualified PeerReview.SubmissionRepo.FPCourseAPIClient as FPCourseAPIClient
import           PeerReview.Types

-- Create submission repo using endpoint configuration.
-- Config is a map containing required endpoints for fetching data.
repo :: SubmissionRepoConfig -> SubmissionRepo
repo cfg =
    let client = FPCourseAPIClient.client
    in repoWithClient client cfg

-- For testing purposes
repoWithClient :: APIClient -> SubmissionRepoConfig -> SubmissionRepo
repoWithClient client cfg =
    SubmissionRepo (byId cfg client) (forTask cfg client) (forUser cfg client) (listAll cfg client)

byId :: SubmissionRepoConfig -> APIClient ->SubmissionID -> IO Submission
byId _ _ _ = return $ Submission "1" "2" "3" (Just "wat")

forTask :: SubmissionRepoConfig -> APIClient -> TaskID -> IO [Submission]
forTask cfg client taskId = do
    allSubmissions <- getAllSubmissions cfg client
    let subsForTask = fmap (V.filter (\s -> sTid s == taskId)) allSubmissions
    return $ maybe [] V.toList subsForTask

forUser :: SubmissionRepoConfig -> APIClient -> UserID -> IO [Submission]
forUser _ _ _ = return []

listAll :: SubmissionRepoConfig -> APIClient -> IO [Submission]
listAll cfg client = do
    allSubmissions <- getAllSubmissions cfg client
    return $ maybe [] V.toList allSubmissions

getAllSubmissions :: SubmissionRepoConfig -> APIClient -> IO (Maybe (Vector Submission))
getAllSubmissions cfg client = do
    jsonResp <- acGetResource client $ unpack (cfg M.! pack "listAllUrl")
    let fpExercises = decode jsonResp :: Maybe (Vector (Vector Text))
        submissions = fmap exercisesToSubmissions fpExercises
    return submissions

-- Submission json structure:
-- [email, exName, submissionID]
exercisesToSubmissions :: Vector (Vector Text) -> Vector Submission
exercisesToSubmissions = fmap (\v -> Submission (v V.! 2) (v V.! 0) (v V.! 1) Nothing)
