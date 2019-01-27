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
    it "EvalT ListToMaybeT" $ do
      testListToMaybeT `shouldBe` Refl


testListToMaybeT :: (EvalT (ListToMaybeT '[1])) :~: 'Just 1
testListToMaybeT = Refl

