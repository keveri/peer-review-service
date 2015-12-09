{-# LANGUAGE OverloadedStrings #-}
module PeerReview.ConfigSpec
  ( main
  , spec
  ) where

import           Test.Hspec

import           PeerReview.Types
import           PeerReview.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readAppConfig" $ do
    it "reads application config port" $ do
      conf <- readAppConfig "test/fixtures/config/test.cfg"
      acPort conf `shouldBe` 1337
    it "reads application DB config" $ do
      conf <- readAppConfig "test/fixtures/config/test.cfg"
      let db = DBInfo "localhost"  4444 "user" "pass" "name"
      acDB conf `shouldBe` db

  describe "readSubmissionRepoConfig" $
    it "fails to read submission repository config" $ do
      let p = "test/fixtures/config/fail.json"
      readSubmissionRepoConfig p `shouldThrow` anyErrorCall
