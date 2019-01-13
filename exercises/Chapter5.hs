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

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

-- Exercise 5.3
instance Ord (HList '[]) where
  compare HNil HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  (a :# as) <= (b :# bs) = a <= b && as <= bs 

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (t :# ts) = show t <> " :# " <> show ts


