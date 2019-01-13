{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes           #-}

module Chapter6 where

newtype Cont a = Cont { unCont :: forall r. (a->r) -> r }


-- Exercise 6.4.i
instance Functor Cont where
  fmap f (Cont c) = Cont $ \callback -> c (callback . f)

-- Exercise 6.4.ii
instance Applicative Cont where
  pure a = Cont $ \callback -> callback a
  Cont f <*> Cont b = Cont $ \c -> f $ \d -> b (c . d)
