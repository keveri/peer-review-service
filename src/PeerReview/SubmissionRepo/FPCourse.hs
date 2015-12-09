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

-- datatype to parse individual
-- fp exercise from json
data FPExercise = FPExercise TaskID Content [UserID] deriving (Show)

instance FromJSON FPExercise where
    parseJSON (Object o) =
        FPExercise <$> o .: "taskID"
                   <*> o .: "text"
                   <*> o .: "students"
    parseJSON _ = mempty

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

byId :: SubmissionRepoConfig -> APIClient -> SubmissionID -> IO (Maybe SubmissionDetails)
byId = getSubmission

forTask :: SubmissionRepoConfig -> APIClient -> TaskID -> IO [Submission]
forTask cfg client taskId = do
    allSubmissions <- getAllSubmissions cfg client
    let subsForTask = fmap (V.filter (\s -> sTid s == taskId)) allSubmissions
    return $ maybe [] V.toList subsForTask

forUser :: SubmissionRepoConfig -> APIClient -> UserID -> IO [Submission]
forUser cfg client userId = do
    allSubmissions <- getAllSubmissions cfg client
    let subsForUser = fmap (V.filter (\s -> sSender s == userId)) allSubmissions
    return $ maybe [] V.toList subsForUser

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

getSubmission :: SubmissionRepoConfig -> APIClient -> SubmissionID -> IO (Maybe SubmissionDetails)
getSubmission cfg client subId = do
    let submissionBaseURL = cfg M.! "byIdUrl"
    jsonResp <- acGetResource client $ unpack (replace ":id" subId submissionBaseURL)
    let fpExercise = decode jsonResp :: Maybe FPExercise
        submission = fmap (exToSubmissionDetails subId) fpExercise
    return submission

exToSubmissionDetails :: SubmissionID -> FPExercise -> SubmissionDetails
exToSubmissionDetails sid (FPExercise tid content students) =
     SubmissionDetails sid tid students content

-- Submission json structure:
-- [email, exName, submissionID]
exercisesToSubmissions :: Vector (Vector Text) -> Vector Submission
exercisesToSubmissions = fmap (\v -> Submission (v V.! 2) (v V.! 0) (v V.! 1))
