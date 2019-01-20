{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter7 where

data Any where
  Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

data HasShow where
  HasShow :: Show t => t -> HasShow

--instance Show HasShow where
--  show (HasShow s) = "HasShow " ++ show s

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

-- Exercise 7.1.iii
instance Show HasShow where
  show = elimHasShow show

  
