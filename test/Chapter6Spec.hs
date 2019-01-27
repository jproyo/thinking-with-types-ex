{-# LANGUAGE DataKinds #-}

module Chapter6Spec (spec) where

import           Chapter6
import           Control.Monad.Identity
import           Test.Hspec


spec :: Spec
spec =
  describe "Chapter6" $ do
    it "Contitnuation Functor instance" $
      (unCont testContFunctor $ id) `shouldBe` 4
    it "Contitnuation Applicative instance" $
      (unCont testContApplicative $ id) `shouldBe` 5
    it "Contitnuation Monad instance" $
      (unCont testContMonad $ id) `shouldBe` "3, 4, 5"
    it "ContitnuationT Functor instance" $
      (runContT testContFunctorT $ pure) `shouldBe` (Identity 4)
    it "ContitnuationT Applicative instance" $
      (runContT testContApplicativeT $ pure) `shouldBe` (Identity 5)
    it "ContitnuationT Monad instance" $
      (runContT testContMonadT $ pure) `shouldBe` (Identity "3, 4, 5")


testContNumberThree :: (Int -> r) -> r
testContNumberThree f = f 3

testContThree :: Cont Int
testContThree = Cont testContNumberThree

testContFunctor :: Cont Int
testContFunctor = (+ 1) <$> testContThree

testContApplicative :: Cont Int
testContApplicative = pure (+ 2) <*> testContThree

testContMonad :: Cont String
testContMonad = do
  numberThree <- testContThree
  numberFour  <- testContFunctor
  numberFive  <- testContApplicative
  return $
    show numberThree <> ", " <> show numberFour <> ", " <> show numberFive

testContNumberThreeI :: (Int -> Identity r) -> Identity r
testContNumberThreeI f = f 3

testContThreeT :: ContT Identity Int
testContThreeT = ContT testContNumberThreeI

testContFunctorT :: ContT Identity Int
testContFunctorT = (+ 1) <$> testContThreeT

testContApplicativeT :: ContT Identity Int
testContApplicativeT = pure (+ 2) <*> testContThreeT

testContMonadT :: ContT Identity String
testContMonadT = do
  numberThree <- testContThreeT
  numberFour  <- testContFunctorT
  numberFive  <- testContApplicativeT
  return $
    show numberThree <> ", " <> show numberFour <> ", " <> show numberFive

