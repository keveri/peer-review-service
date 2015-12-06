{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourse
    ( repo
    ) where

import           PeerReview.Types
import           Data.Aeson
import           Control.Lens
import           Data.Vector                     as V
import           Data.Map                        as M
import           Network.Wreq
import           Data.ByteString.Lazy.Internal
import           Data.Text

-- Create submission repo using endpoint configuration.
-- Config is a map containing required endpoints for fetching data.
repo :: SubmissionRepoConfig -> SubmissionRepo
repo cfg = SubmissionRepo (byId cfg) (forTask cfg) (forUser cfg) (listAll cfg)

byId :: SubmissionRepoConfig -> SubmissionID -> IO Submission
byId _ _ = return $ Submission "1" "2" "3" (Just "wat")

forTask :: SubmissionRepoConfig -> TaskID -> IO [Submission]
forTask _ _ = return []

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
    let fpExercises = decode jsonResp :: Maybe (Vector (Vector String))
        submissions = fmap exToSubmission <$> fpExercises
    return submissions

-- submission json structure:
-- [email, exName, submissionID]
exToSubmission :: Vector String -> Submission
exToSubmission v =
    Submission (pack (v V.! 2)) (pack (v V.! 0)) (pack (v V.! 1)) Nothing

getJSONResource :: String -> IO ByteString
getJSONResource url = do
    r <- get url
    let [body] = r ^.. responseBody
    return body
