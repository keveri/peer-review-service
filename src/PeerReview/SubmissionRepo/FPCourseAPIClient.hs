{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourseAPIClient
    ( client
    ) where

import           Control.Lens
import           Data.ByteString.Lazy.Internal
import           Network.Wreq

import           PeerReview.Types

client :: APIClient
client = APIClient getResource

getResource :: String -> IO ByteString
getResource url = do
    r <- get url
    let [body] = r ^.. responseBody
    return body
