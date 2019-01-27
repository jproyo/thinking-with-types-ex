{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Chapter7Spec (spec) where

import           Chapter7
import           Test.Hspec


spec :: Spec
spec =
  describe "Chapter7" $ do
    it "Elimination of HasShow Instance" $ do
      show hasShowInst `shouldBe` "3"

hasShowInst :: HasShow
hasShowInst = HasShow @Integer 3



