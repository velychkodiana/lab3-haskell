module Main where

import Test.Hspec
import TextProcessing

main :: IO ()
main = hspec $ do

  describe "normalizeSpaces" $ do
    it "replaces multiple spaces with single" $
      normalizeSpaces "Haskell   is   cool" `shouldBe` "Haskell is cool"

  describe "splitIntoSentences" $ do
    it "splits into sentences" $
      map showSentence (splitIntoSentences "Hello world! Bye.") `shouldBe`
        ["Hello world!", "Bye."]

  describe "wordCount" $ do
    it "counts words correctly" $
      wordCount (head (splitIntoSentences "Hello world!")) `shouldBe` 2
