{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Engine.Generator 
  ( Generator(..)
  , HasGenerator(..)
  -- * Cache
  , Cache(..)
  , HasCache(..)
  , emptyCache
  , generate
  , generates
  , delete
  -- * Caches
  , Caches(..)
  , HasCaches(..)
  , emptyCaches
  ) where


import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Traversable
import Data.Typeable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Core31

data Generator = Generator
  { _generatorGen :: GLsizei -> Ptr GLuint -> IO ()
  , _generatorDel :: GLsizei -> Ptr GLuint -> IO ()
  } deriving Typeable

makeClassy ''Generator

textureGenerator, bufferGenerator, queryGenerator, renderbufferGenerator, vertexArrayGenerator :: Generator
textureGenerator = Generator glGenTextures glDeleteTextures
bufferGenerator = Generator glGenBuffers glDeleteBuffers
queryGenerator = Generator glGenQueries glDeleteQueries
renderbufferGenerator = Generator glGenRenderbuffers glDeleteRenderbuffers
vertexArrayGenerator = Generator glGenVertexArrays glDeleteVertexArrays

pool :: Generator -> Int -> IO [GLuint]
pool g n = allocaBytesAligned (alignment (undefined :: GLuint)) (sizeOf (undefined :: GLuint) * n)  $ \p -> do
  _generatorGen g (fromIntegral n) p
  for [0..n-1] $ peekElemOff p

data Cache = Cache { _cacheGenerator :: Generator, _cacheSize :: Int, _cachePool :: [GLuint] }
  deriving Typeable

makeClassy ''Cache

instance HasGenerator Cache where
  generator = cacheGenerator

emptyCache :: Generator -> Int -> Cache 
emptyCache g n = Cache g n []

-- | Generate an OpenGL resource, leaning on a local a cache
generate :: (MonadIO m, MonadState s m) => ALens' s Cache -> m GLuint
generate c = use (cloneLens c) >>= \ (Cache g s p) -> case p of
  [] -> do 
    (x:xs) <- liftIO (pool g s)
    cloneLens c.cachePool .= xs
    return x 
  (x:xs) -> do
    cloneLens c.cachePool .= xs
    return x

-- | Generate many OpenGL resources, leaning on a local a cache
generates :: (MonadIO m, MonadState s m) => ALens' s Cache -> Int -> m [GLuint]
generates c n = use (cloneLens c) >>= \ (Cache g s p) -> case splitAt n p of
    (xs,ys) 
      | l == n -> do
        cloneLens c.cachePool .= ys
        return xs
      | otherwise -> do
        cs <- liftIO (pool g $ max s $ n-l)
        let (as,bs) = splitAt (n-l) cs
        cloneLens c.cachePool .= bs
        return $ p ++ as
      where l = length xs

-- | Return an OpenGL resource to the cache
-- 
-- TODO: delete the pool if its large enough
delete :: MonadState s m => ALens' s Cache -> GLuint -> m ()
delete c i = cloneLens c.cachePool %= (i:)

-- | Caches for several OpenGL resource types
data Caches = Caches
  { _textures, _buffers, _queries, _renderbuffers, _vertexArrays :: !Cache
  } deriving Typeable

emptyCaches :: Caches
emptyCaches = Caches
  { _textures = emptyCache textureGenerator 1024
  , _buffers = emptyCache bufferGenerator 64
  , _queries = emptyCache queryGenerator 1024
  , _renderbuffers = emptyCache renderbufferGenerator 32
  , _vertexArrays = emptyCache vertexArrayGenerator 1024
  }

makeClassy ''Caches
