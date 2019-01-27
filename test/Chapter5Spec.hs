{-# LANGUAGE DataKinds #-}

module Chapter5Spec (spec) where

import           Chapter5
import           Test.Hspec

spec :: Spec
spec =
  describe "Chapter5" $ do
    it "shows HList properly" $ do
      show testDataTwoElem `shouldBe` expectedShowTestDataTwoElem
    it "should be equal if 2 HList contains same Elements and Types" $ do
      testDataTwoElem == testDataTwoElem `shouldBe` True
    it
      "should not be equal if 2 HList contains different Elements but same amount of Types" $ do
      testDataTwoElem == testDataOtherTwoElem `shouldBe` False
    it
      "should not be equal if 2 HList contains different Elements but Different Types" $ do
      testDataTwoElem == testDataOtherTwoElem `shouldBe` False
    it
      "should be greater if has more elements in list" $ do
      testDataTwoElemWithGreaterMaybe > testDataTwoElem `shouldBe` True
    it
      "should be less if has less elements in list" $ do
      testDataTwoElem < testDataTwoElemWithGreaterMaybe `shouldBe` True

expectedShowTestDataTwoElem :: String
expectedShowTestDataTwoElem = "Just 1 :# True :# HNil"

testDataTwoElem :: HList '[Maybe Integer, Bool]
testDataTwoElem = Just 1 :# True :# HNil

testDataTwoElemWithGreaterMaybe :: HList '[Maybe Integer, Bool]
testDataTwoElemWithGreaterMaybe = Just 2 :# True :# HNil

testDataOtherTwoElem :: HList '[Maybe a, Bool]
testDataOtherTwoElem = Nothing :# True :# HNil

