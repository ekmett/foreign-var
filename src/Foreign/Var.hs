{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014-2015 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Foreign.Var
  ( HasGetter(get)
  , GettableVar
  , HasSetter(($=))
  , SettableVar(SettableVar)
  , ($=!)
  , Var(Var)
  , mapVar
  , HasUpdate(($~), ($~!))
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
--import Data.Void
--import Data.Functor.Contravariant
--import Data.Functor.Contravariant.Divisible

infixr 2 $=, $=!, $~, $~!

class HasSetter t a | t -> a where
  ($=) :: MonadIO m => t -> a -> m ()

instance Storable a => HasSetter (Ptr a) a where
  p $= a = liftIO $ poke p a
  {-# INLINE ($=) #-}

instance HasSetter (IORef a) a where
  p $= a = liftIO $ writeIORef p a
  {-# INLINE ($=) #-}

instance HasSetter (TVar a) a where
  p $= a = liftIO $ atomically $ writeTVar p a

($=!) :: (HasSetter t a, MonadIO m) => t -> a -> m ()
p $=! a = (p $=) $! a
{-# INLINE ($=!) #-}

newtype SettableVar a = SettableVar (a -> IO ()) deriving Typeable

{-
instance Contravariant SettableVar where
  contramap f (SettableVar k) = SettableVar (k . f)
  {-# INLINE contramap #-}

instance Divisible SettableVar where
  divide k (SettableVar l) (SettableVar r) = SettableVar $ \ a -> case k a of
    (b, c) -> l b >> r c
  conquer = SettableVar $ \_ -> return ()

instance Decidable SettableVar where
  lose k = SettableVar (absurd . k)
  choose k (SettableVar l) (SettableVar r) = SettableVar $ \ a -> case k a of
    Left b -> l b
    Right c -> r c
-}

instance HasSetter (SettableVar a) a where
  SettableVar f $= a = liftIO (f a)
  {-# INLINE ($=) #-}

type GettableVar = IO

class HasGetter t a | t -> a where
  get :: MonadIO m => t -> m a

instance HasGetter (TVar a) a where
  get = liftIO . atomically . readTVar

instance HasGetter (IO a) a where
  get = liftIO

instance HasGetter (STM a) a where
  get = liftIO . atomically

instance Storable a => HasGetter (Ptr a) a where
  get = liftIO . peek

instance HasGetter (IORef a) a where
  get = liftIO . readIORef

data Var a = Var (IO a) (a -> IO ()) deriving Typeable

instance HasGetter (Var a) a where
  get (Var g _) = liftIO g

instance HasSetter (Var a) a where
  Var _ s $= a = liftIO $ s a

mapVar :: (b -> a) -> (a -> b) -> Var a -> Var b
mapVar ba ab (Var ga sa) = Var (ab <$> ga) (sa . ba)
{-# INLINE mapVar #-}

class HasUpdate t a | t -> a where
  ($~) :: MonadIO m => t -> (a -> a) -> m ()
  default ($~) :: (MonadIO m, HasGetter t a, HasSetter t a) => t -> (a -> a) -> m ()
  r $~ f = liftIO $ do
    a <- get r
    r $= f a
  ($~!) :: MonadIO m => t -> (a -> a) -> m ()
  default ($~!) :: (MonadIO m, HasGetter t a, HasSetter t a) => t -> (a -> a) -> m ()
  r $~! f = liftIO $ do
    a <- get r
    r $=! f a

instance HasUpdate (Var a) a

instance Storable a => HasUpdate (Ptr a) a

instance HasUpdate (IORef a) a where
  r $~ f  = liftIO $ atomicModifyIORef r $ \a -> (f a,())
  r $~! f = liftIO $ atomicModifyIORef' r $ \a -> (f a,())

instance HasUpdate (TVar a) a where
  r $~ f = liftIO $ atomically $ do
    a <- readTVar r
    writeTVar r (f a)
  r $~! f = liftIO $ atomically $ do
    a <- readTVar r
    writeTVar r $! f a
