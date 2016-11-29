module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

  describe "empty pattern" $ do
    it "matches empty string" $
      matchGlob "" "" `shouldBe` True
    it "shouldn't match non-empty string" $
      matchGlob "" "string" `shouldBe` False

  describe "combine patters" $ do
      it "matches string with multiple patterns" $
        matchGlob
          "q****we?rty*\\**STOP*[abcA-F]*\\?"
          "qwe_rty______*___STOP___D___?"
          `shouldBe` True
 