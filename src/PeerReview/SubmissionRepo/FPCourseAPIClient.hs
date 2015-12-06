{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourseAPIClient
    (
    client
    ) where

import           PeerReview.Types
import           Network.Wreq
import           Control.Lens
import           Data.ByteString.Lazy.Internal

client :: APIClient
client = APIClient getResource

getResource :: String -> IO ByteString
getResource url = do
    r <- get url
    let [body] = r ^.. responseBody
    return body
