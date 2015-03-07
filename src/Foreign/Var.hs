{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
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
  (
  -- * Variables
    Var(Var)
  , newVar
  , mapVar
  , SettableVar(SettableVar)
  , GettableVar
  -- * Classes
  , HasSetter(($=)), ($=!)
  , HasUpdate(($~), ($~!))
  , HasGetter(get)
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

--------------------------------------------------------------------
-- * Var
--------------------------------------------------------------------

data Var a = Var
  (IO a)              -- get
  ((a -> a) -> IO ()) -- update
  ((a -> a) -> IO ()) -- strict update
  (a -> IO ())        -- set
  deriving Typeable

-- | Build a 'Var' form a getter and a setter.
newVar :: (IO a)       -- ^ getter
       -> (a -> IO ()) -- ^ setter
       -> Var a
newVar g s = Var g u u' s where
  u f = do
    a <- g
    s (f a)
  u' f = do
    a <- g
    s $! f a

-- | Change the type of a 'Var'
mapVar :: (b -> a) -> (a -> b) -> Var a -> Var b
mapVar ba ab (Var ga ua ua' sa) = Var (ab <$> ga) (\bb -> ua (ba . bb . ab)) (\bb -> ua' (ba . bb . ab)) (sa . ba)
{-# INLINE mapVar #-}

newtype SettableVar a = SettableVar (a -> IO ())
  deriving Typeable

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

type GettableVar = IO

--------------------------------------------------------------------
-- * HasSetter
--------------------------------------------------------------------

infixr 2 $=, $=!

class HasSetter t a | t -> a where
  ($=) :: MonadIO m => t -> a -> m ()

($=!) :: (HasSetter t a, MonadIO m) => t -> a -> m ()
p $=! a = (p $=) $! a
{-# INLINE ($=!) #-}

instance HasSetter (SettableVar a) a where
  SettableVar f $= a = liftIO (f a)
  {-# INLINE ($=) #-}

instance HasSetter (Var a) a where
  Var _ _ _ s $= a = liftIO $ s a

instance Storable a => HasSetter (Ptr a) a where
  p $= a = liftIO $ poke p a
  {-# INLINE ($=) #-}

instance HasSetter (IORef a) a where
  p $= a = liftIO $ writeIORef p a
  {-# INLINE ($=) #-}

instance HasSetter (TVar a) a where
  p $= a = liftIO $ atomically $ writeTVar p a

--------------------------------------------------------------------
-- * HasUpdate
--------------------------------------------------------------------

infixr 2 $~, $~!

class HasSetter t a => HasUpdate t a b | t -> a b where
  ($~) :: MonadIO m => t -> (a -> b) -> m ()
  default ($~) :: (MonadIO m, a ~ b, HasGetter t a, HasSetter t a) => t -> (a -> b) -> m ()
  r $~ f = liftIO $ do
    a <- get r
    r $= f a
  ($~!) :: MonadIO m => t -> (a -> b) -> m ()
  default ($~!) :: (MonadIO m, a ~ b, HasGetter t a, HasSetter t a) => t -> (a -> b) -> m ()
  r $~! f = liftIO $ do
    a <- get r
    r $=! f a

instance HasUpdate (Var a) a a where
  Var _ u _ _ $~  f = liftIO $ u f
  Var _ _ v _ $~! f = liftIO $ v f

instance Storable a => HasUpdate (Ptr a) a a

instance HasUpdate (IORef a) a a where
  r $~ f  = liftIO $ atomicModifyIORef r $ \a -> (f a,())
  r $~! f = liftIO $ atomicModifyIORef' r $ \a -> (f a,())

instance HasUpdate (TVar a) a a where
  r $~ f = liftIO $ atomically $ do
    a <- readTVar r
    writeTVar r (f a)
  r $~! f = liftIO $ atomically $ do
    a <- readTVar r
    writeTVar r $! f a

--------------------------------------------------------------------
-- * HasGetter
--------------------------------------------------------------------

class HasGetter t a | t -> a where
  get :: MonadIO m => t -> m a

instance HasGetter (Var a) a where
  get (Var g _ _ _) = liftIO g

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
