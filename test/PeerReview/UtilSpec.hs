{-# LANGUAGE OverloadedStrings #-}
module PeerReview.UtilSpec
  ( main
  , spec
  ) where

import           Test.Hspec

import           PeerReview.Types
import           PeerReview.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "reviewFromSub" $
    it "Constructs an peer review from user id and submission data" $
      let uid         = "0"
          submission  = Submission "1" "2" "3"
          expectation = PeerReview "1" "3" "" 0 "0" Waiting
      in expectation `shouldBe` reviewFromSub uid submission
