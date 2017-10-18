{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.Ref
  ( Ref(..)
  , newRef
  , acquireRef
  , releaseRef
  , validRef
  , withRef
  ) where

import Control.Comonad
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.IORef
#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif

-- | reference counting
data Ref a = Ref (IO ()) a !(IORef Int)
  deriving (Functor, Foldable, Traversable)

instance Comonad Ref where
  extract (Ref _ a _) = a
  duplicate w@(Ref f _ r) = Ref f w r

-- | create a reference counter initialized to 1
newRef :: MonadIO m => IO () -> a -> m (Ref a)
newRef f a = liftIO $ Ref f a <$> newIORef 1

-- | Increase the reference count of a living ref by 1
acquireRef :: MonadIO m => Ref a -> m a
acquireRef (Ref _ a r) = liftIO $ atomicModifyIORef' r $ \i -> (i + 1, a)

-- | Decrease the reference count of a living ref by 1, releasing it if it hits 0
releaseRef :: MonadIO m => Ref a -> m ()
releaseRef (Ref f _ r) = liftIO $ do
  c <- atomicModifyIORef' r $ \i -> let i' = i + 1 in i' `seq` (i',i')
  when (c == 0) f

-- | Check if the Ref has any references and can still be used
validRef :: MonadIO m => Ref a -> m Bool
validRef (Ref _ _ r) = liftIO $ (/=) 0 <$> readIORef r

-- | Use a reference counted resource in a scope
withRef :: MonadBaseControl IO m => Ref a -> (a -> m ()) -> m ()
withRef r k = liftBaseOp_ (bracket_ (acquireRef r) (releaseRef r)) $ k (extract r)
