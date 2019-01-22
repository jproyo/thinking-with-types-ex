{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter5 where

import Data.Kind (Constraint, Type)


data HList (ts::[Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts) 
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) =t

showBool :: HList '[_1, Bool, _2] -> String
showBool( _ :# b :# _ :# HNil) = show b

{-
instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

-- Exercise 5.3.i
instance Ord (HList '[]) where
  compare HNil HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  (a :# as) <= (b :# bs) = a <= b && as <= bs 

-- Exercise 5.3.ii
instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (t :# ts) = show t <> " :# " <> show ts
-}

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

-- Exercise 5.3.iii
instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  HNil <= HNil = True
  (a :# as) <= (b :# bs) = a <= b && as <= bs

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (a :# as) = show a <> " :# " <> show as

testDataTwoElem :: HList '[Maybe Integer, Bool]
testDataTwoElem = Just 1 :# True :# HNil

testDataTwoElemWithGreaterMaybe :: HList '[Maybe Integer, Bool]
testDataTwoElemWithGreaterMaybe = Just 2 :# True :# HNil

testDataOtherTwoElem :: HList '[Maybe a, Bool]
testDataOtherTwoElem = Nothing :# True :# HNil

testAllShow :: String
testAllShow = show testDataTwoElem

testEqTrue :: Bool
testEqTrue = testDataTwoElem == testDataTwoElem

testEqFalse :: Bool
testEqFalse = testDataTwoElem == testDataOtherTwoElem

testGreaterThan :: Bool
testGreaterThan = testDataTwoElemWithGreaterMaybe > testDataTwoElem

testLessThan :: Bool
testLessThan = testDataTwoElem < testDataTwoElemWithGreaterMaybe 
