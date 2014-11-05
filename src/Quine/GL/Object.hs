module Quine.GL.Object
  ( Object(..)
  , Gen(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Graphics.GL.Raw.Types

class Object a where
  {-# MINIMAL object, isa, (delete | deletes) #-}
  object :: a -> GLuint

  isa :: MonadIO m => a -> m Bool

  delete :: MonadIO m => a -> m ()
  delete = deletes . return

  deletes :: MonadIO m => [a] -> m ()
  deletes xs = liftIO $ forM_ xs delete

class Object a => Gen a where
  {-# MINIMAL gen | gens #-}
  gen :: MonadIO m => m a
  gen = liftIO $ head <$> gens 1

  gens :: MonadIO m => Int -> m [a]
  gens n = liftIO $ replicateM n gen
