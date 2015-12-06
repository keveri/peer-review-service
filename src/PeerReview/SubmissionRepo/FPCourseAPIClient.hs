{-# LANGUAGE OverloadedStrings #-}
module PeerReview.SubmissionRepo.FPCourseAPIClient
    (
    getJSONResource
    ) where

import           Network.Wreq
import           Control.Lens
import           Data.ByteString.Lazy.Internal

getJSONResource :: String -> IO ByteString
getJSONResource url = do
    r <- get url
    let [body] = r ^.. responseBody
    return body
