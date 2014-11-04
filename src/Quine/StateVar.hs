{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Quine.StateVar
  ( HasGetter(get)
  , GettableStateVar
  , HasSetter(($=))
  , SettableStateVar(SettableStateVar)
  , ($=!)
  , StateVar(StateVar)
  , mapStateVar
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Functor
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.IORef
import Data.Typeable
import Data.Void
import Foreign.Ptr
import Foreign.Storable

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

newtype SettableStateVar a = SettableStateVar (a -> IO ()) deriving Typeable

instance Contravariant SettableStateVar where
  contramap f (SettableStateVar k) = SettableStateVar (k . f)
  {-# INLINE contramap #-}

instance Divisible SettableStateVar where
  divide k (SettableStateVar l) (SettableStateVar r) = SettableStateVar $ \ a -> case k a of
    (b, c) -> l b >> r c
  conquer = SettableStateVar $ \_ -> return ()

instance Decidable SettableStateVar where
  lose k = SettableStateVar (absurd . k)
  choose k (SettableStateVar l) (SettableStateVar r) = SettableStateVar $ \ a -> case k a of
    Left b -> l b
    Right c -> r c

instance HasSetter (SettableStateVar a) a where
  SettableStateVar f $= a = liftIO (f a)
  {-# INLINE ($=) #-}

type GettableStateVar = IO

class HasGetter t a | t -> a where
  get :: MonadIO m => t -> m a

instance HasGetter (TVar a) a where
  get = liftIO . atomically . readTVar

instance HasGetter (IO a) a where
  get = liftIO

instance Storable a => HasGetter (Ptr a) a where
  get = liftIO . peek

instance HasGetter (IORef a) a where
  get = liftIO . readIORef

data StateVar a = StateVar (IO a) (a -> IO ()) deriving Typeable

instance HasGetter (StateVar a) a where
  get (StateVar g _) = liftIO g

instance HasSetter (StateVar a) a where
  StateVar _ s $= a = liftIO $ s a

mapStateVar :: (b -> a) -> (a -> b) -> StateVar a -> StateVar b
mapStateVar ba ab (StateVar ga sa) = StateVar (ab <$> ga) (sa . ba)
{-# INLINE mapStateVar #-}
