{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Chapter10Spec (spec) where

import           Chapter10
import           Data.Type.Equality
import           Test.Hspec

spec :: Spec
spec =
  describe "Chapter10" $ do
    it "Defunctionalise ListToMaybe type" $ do
      eval (ListToMaybe @Integer [1, 2, 3]) `shouldBe` Just 1
      eval (ListToMaybe @Integer []) `shouldBe` Nothing
    it "Defunctionalise ListToMaybeT true" $ testListToMaybeT `shouldBe` Refl
    it "Defunctionalise ListToMaybeT false" $ testListToMaybeTFalse `shouldBe` Refl
    it "Defunctionalise FoldR true" $ testFoldR `shouldBe` Refl
    it "Defunctionalise FoldR false" $ testFoldRFalse `shouldBe` Refl
    it "Defunctionalise Map over (,)" $ testProdTypeMap `shouldBe` Refl

testListToMaybeT :: ((EvalT (ListToMaybeT '[1])) == 'Just 1) :~: 'True
testListToMaybeT = Refl

testListToMaybeTFalse :: ((EvalT (ListToMaybeT '[])) == 'Just 1) :~: 'False
testListToMaybeTFalse = Refl

testFoldR :: ((EvalT (FoldR ConsR 'True '[False, True, False])) == 'False) :~: 'True
testFoldR = Refl

testFoldRFalse :: ((EvalT (FoldR ConsR 'True '[])) == 'False) :~: 'False
testFoldRFalse = Refl

testProdTypeMap :: ((EvalT (Map Idt '(Nothing, Just 2))) == '(Nothing, Just 2)) :~: 'True
testProdTypeMap = Refl


