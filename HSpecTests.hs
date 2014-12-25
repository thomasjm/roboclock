module Main where

import Test.Hspec
import Control.Monad (forM)

import Numbers (numbers, traceFnFromPoints)

main :: IO ()
main = hspec $ do
  describe "traceFnFromPoints" $ do
    it "Should go saturate above at 1.0" $ do
      traceFnFromPoints pts 2.0 `shouldBe` (1.0, 1.0)

    it "Should go saturate below at 0.0" $ do
      traceFnFromPoints pts (-2.0) `shouldBe` (0.0, 0.0)

    it "Should sanity check " $ do
      foldl1 (>>) [traceFnFromPoints pts x `shouldBe` (x, x) | x <- distances] where
        distances = [0.0, 0.3, 0.5, 0.7, 1.0]
        pts = [(0, 0), (0.5, 0.5), (1, 1)]
