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

-- Exercise 6.4.iii
instance Monad Cont where
  Cont a >>= f = Cont $ \callback -> a $ \b -> unCont (f b) callback

-- Exercise 6.4.iv
newtype ContT m a = ContT { runContT :: forall r. (a -> m r) -> m r }

instance Functor (ContT m) where
  fmap f (ContT c) = ContT $ \callback -> c (callback . f)

instance Applicative (ContT m) where
  pure a = ContT $ \callback -> callback a
  ContT f <*> ContT b = ContT $ \c -> f $ \d -> b (c . d)
  
instance Monad (ContT m) where
  ContT a >>= f = ContT $ \callback -> a $ \b -> runContT (f b) callback 
