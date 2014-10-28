{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.GL.Cache
  ( 
  -- * Cache
    Cache(..)
  , Caches(..)
  , HasCaches(..)
  , Cached(..)
  , generate
  , generates
  , delete
  , purge
  -- * exposed internals
  , cachePool
  , cacheSize
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Data
import Data.Default
import Data.Foldable
import GHC.Generics
import Graphics.Rendering.OpenGL

data Cache a = Cache { _cachePool :: [a], _cacheSize :: Int }
  deriving (Typeable, Data, Generic, Functor, Foldable, Traversable)

makeLenses ''Cache

-- | Caches for several OpenGL resource types
data Caches = Caches
  { _textures      :: !(Cache TextureObject)
  , _buffers       :: !(Cache BufferObject)
  , _queries       :: !(Cache QueryObject)
  , _renderbuffers :: !(Cache RenderbufferObject)
  , _vertexArrays  :: !(Cache VertexArrayObject)
  }

makeClassy ''Caches

class GeneratableObjectName a => Cached a where
  cache :: HasCaches t => Lens' t (Cache a)

instance Cached TextureObject where cache = textures
instance Cached BufferObject where cache = buffers
instance Cached QueryObject where cache = queries
instance Cached RenderbufferObject where cache = renderbuffers
instance Cached VertexArrayObject where cache = vertexArrays

instance Default Caches where
  def = Caches
    { _textures      = Cache [] 1024
    , _buffers       = Cache [] 64
    , _queries       = Cache [] 1024
    , _renderbuffers = Cache [] 32
    , _vertexArrays  = Cache [] 1024
    }

-- | Generate an OpenGL resource, leaning on a local a cache
generate :: (MonadIO m, MonadState s m, HasCaches s, Cached a) => m a
generate = use cache >>= \ (Cache p s) -> case p of
  [] -> do 
    (x:xs) <- liftIO $ genObjectNames s
    cache.cachePool .= xs
    return x 
  (x:xs) -> do
    cache.cachePool .= xs
    return x

-- | Generate many OpenGL resources, leaning on a local a cache
generates :: (MonadIO m, MonadState s m, HasCaches s, Cached a) => Int -> m [a]
generates n = use cache >>= \ (Cache p s) -> case splitAt n p of
  (xs,ys) 
    | l == n -> do
      cache.cachePool .= ys
      return xs
    | otherwise -> do
      cs <- liftIO (genObjectNames $ max s $ n-l)
      let (as,bs) = splitAt (n-l) cs
      cache.cachePool .= bs
      return $ p ++ as
    where l = length xs

-- | Return an OpenGL resource to the cache
delete :: (MonadState s m, HasCaches s, Cached a) => a -> m ()
delete i = cache.cachePool %= (i:)

-- | Purge a cache
purge :: (MonadState s m, MonadIO m, ObjectName a) => ALens' s (Cache a) -> m ()
purge c = do
  xs <- cloneLens c.cachePool <<.= []
  liftIO $ deleteObjectNames xs
