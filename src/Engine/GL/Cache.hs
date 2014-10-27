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
module Engine.GL.Cache
  ( 
  -- * Cache
    Cache(..)
  , HasCache(..)
  , generate
  , generates
  , delete
  , purge
  -- * Caches
  , Caches(..)
  , HasCaches(..)
  ) where


import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Data
import Data.Default
import Data.Foldable
import Data.Functor
import GHC.Generics
import Graphics.Rendering.OpenGL

data Cache a = Cache { _cachePool :: [a], _cacheSize :: Int }
  deriving (Typeable, Data, Generic, Functor, Foldable, Traversable)

class HasCache t a where
  cache :: Lens' t (Cache a)
  cachePool :: Lens' t [a]
  cachePool = cache.cachePool

instance a ~ b => HasCache (Cache a) b where
  cache = id
  cachePool f (Cache xs n) = (`Cache` n) <$> f xs

-- | Generate an OpenGL resource, leaning on a local a cache
generate :: (MonadIO m, MonadState s m, GeneratableObjectName a, HasCache s a) => m a
generate = use cache >>= \ (Cache p s) -> case p of
  [] -> do 
    (x:xs) <- liftIO $ genObjectNames s
    cachePool .= xs
    return x 
  (x:xs) -> do
    cachePool .= xs
    return x

-- | Generate many OpenGL resources, leaning on a local a cache
generates :: (MonadIO m, MonadState s m, GeneratableObjectName a, HasCache s a) => Int -> m [a]
generates n = use cache >>= \ (Cache p s) -> case splitAt n p of
  (xs,ys) 
    | l == n -> do
      cachePool .= ys
      return xs
    | otherwise -> do
      cs <- liftIO (genObjectNames $ max s $ n-l)
      let (as,bs) = splitAt (n-l) cs
      cachePool .= bs
      return $ p ++ as
    where l = length xs

-- | Return an OpenGL resource to the cache
delete :: (MonadState s m, HasCache s a) => a -> m ()
delete i = cachePool %= (i:)

-- | Purge a cache
purge :: (MonadState s m, MonadIO m, ObjectName a) => ALens' s (Cache a) -> m ()
purge c = do
  xs <- cloneLens c.cachePool <<.= []
  liftIO $ deleteObjectNames xs

-- | Caches for several OpenGL resource types
data Caches = Caches
  { _textures      :: !(Cache TextureObject)
  , _buffers       :: !(Cache BufferObject)
  , _queries       :: !(Cache QueryObject)
  , _renderbuffers :: !(Cache RenderbufferObject)
  , _vertexArrays  :: !(Cache VertexArrayObject)
  }

makeClassy ''Caches

instance HasCache Caches TextureObject      where cache = textures
instance HasCache Caches BufferObject       where cache = buffers
instance HasCache Caches QueryObject        where cache = queries
instance HasCache Caches RenderbufferObject where cache = renderbuffers
instance HasCache Caches VertexArrayObject  where cache = vertexArrays

instance Default Caches where
  def = Caches
    { _textures      = Cache [] 1024
    , _buffers       = Cache [] 64
    , _queries       = Cache [] 1024
    , _renderbuffers = Cache [] 32
    , _vertexArrays  = Cache [] 1024
    }
