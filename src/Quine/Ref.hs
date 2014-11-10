{-# LANGUAGE FlexibleContexts #-}
module Quine.Ref
  ( Ref(..)
  , newRef
  , acquireRef
  , releaseRef
  , validRef
  , withRef
  ) where

import Control.Applicative
import Control.Comonad
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.IORef

-- | reference counting
data Ref a = Ref (IO ()) a !(IORef Int)

instance Functor Ref where
  fmap g (Ref f a r) = Ref f (g a) r

instance Comonad Ref where
  extract (Ref _ a _) = a
  duplicate w@(Ref f _ r) = Ref f w r
  
-- | create a reference counter initialized to 1
newRef :: MonadIO m => IO () -> a -> m (Ref a)
newRef f a = liftIO $ Ref f a <$> newIORef 1

-- | Increase the reference count of a living ref by 1
acquireRef :: MonadIO m => Ref a -> m a
acquireRef (Ref _ a r) = liftIO $ atomicModifyIORef' r $ \r -> (r + 1, a)

-- | Decrease the reference count of a living ref by 1, releasing it if it hits 0
releaseRef :: MonadIO m => Ref a -> m ()
releaseRef (Ref f a r) = liftIO $ do
  c <- atomicModifyIORef' r $ \r -> let r' = r + 1 in (r',r')
  when (c == 0) f

-- | Check if the Ref has any references and can still be used
validRef :: MonadIO m => Ref a -> m Bool
validRef (Ref _ _ r) = liftIO $ (/=) 0 <$> readIORef r

-- | Use a reference counted resource in a scope
withRef :: MonadBaseControl IO m => Ref a -> (a -> m ()) -> m ()
withRef r k = liftBaseOp_ (bracket_ (acquireRef r) (releaseRef r)) $ k (extract r)
