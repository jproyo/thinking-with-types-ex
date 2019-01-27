{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter10 where

import Prelude hiding (fst)
import Data.Kind (Type)

class Eval l t | l -> t where
  eval :: l -> t

-- Exercise 10.1.i
data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe []) = Nothing
  eval (ListToMaybe (a : _)) = Just a


type Exp a = a -> Type

type family EvalT (e :: Exp a) :: a

-- Exercise 10.2.i
data ListToMaybeT :: [a] -> Exp (Maybe a)
type instance EvalT (ListToMaybeT '[]) = 'Nothing
type instance EvalT (ListToMaybeT (x ': _)) = 'Just x

-- Exercise 10.2.ii
data FoldR :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance EvalT (FoldR _ _ '[]) = '[]
type instance EvalT (FoldR f b (a ': as)) = EvalT (f a (EvalT (FoldR f b as)))

data ConsR :: a -> b -> Exp a
type instance EvalT (ConsR a _) = a

data Map :: (a -> Exp b) -> fa -> Exp (fb)

-- Exercise 10.4.i
type instance EvalT (Map f '(a, b)) = '(a, EvalT (f b))

