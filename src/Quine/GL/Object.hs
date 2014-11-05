module Quine.GL.Object
  ( Object(..)
  , Gen(..)
  ) where

class Object t where
  {-# MINIMAL objectId, isa, (delete | deletes) #-}
  objectId :: a -> GLuint

  isa :: MonadIO m => a -> m Bool

  delete :: MonadIO m => a -> m ()
  delete = deletes . pure

  deletes :: MonadIO m => [a] -> m ()
  deletes xs = liftIO $ forM_ xs delete

class Object t => Gen t where
  {-# MINIMAL gen | gens #-}
  gen :: MonadIO m => m a
  gen = liftIO $ head <$> gens 1

  gens :: MonadIO m => Int -> m [a]
  gens n = liftIO $ replicateM n gen
