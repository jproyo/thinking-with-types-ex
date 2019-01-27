{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeInType       #-}
{-# LANGUAGE TypeOperators    #-}

module Chapter10Spec (spec) where

import           Chapter10
import           Data.Type.Equality
import           Test.Hspec

spec :: Spec
spec =
  describe "Chapter10" $ do
    it "EvalT ListToMaybeT true" $ testListToMaybeT `shouldBe` Refl
    it "EvalT ListToMaybeT false" $ testListToMaybeTFalse `shouldBe` Refl


testListToMaybeT :: ((EvalT (ListToMaybeT '[1])) == 'Just 1) :~: 'True
testListToMaybeT = Refl

testListToMaybeTFalse :: ((EvalT (ListToMaybeT '[])) == 'Just 1) :~: 'False
testListToMaybeTFalse = Refl

