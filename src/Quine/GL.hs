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
module Quine.GL
  ( 
  -- * Shader compilation environment
    ShaderEnv
  , HasShaderEnv(..)
  , buildShaderEnv
  -- * Compiling and Linking
  , compile
  , link
  -- * Exceptions
  , ProgramException(..), _ProgramException
  , ShaderException(..), _ShaderException
  -- * Internals
  , cpp
  , defined
  -- * Caching Resources
  , Cache(..)
  , Caches(..)
  , HasCaches(..)
  , Cached(..)
  , generate
  , generates
  , delete
  , purge
  -- * Exposed Internals
  , cachePool
  , cacheSize
  -- * State variables
  , (&=), (&=!)
  , the
  , xmap
  ) where

import Control.Applicative
import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class hiding (get)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Data
import Data.Default
import Data.Foldable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.ARB.ES2Compatibility
import Graphics.Rendering.OpenGL.Raw.ARB.FragmentShader
import Language.Preprocessor.Cpphs
import Prelude hiding (concat)
import Quine.Options
import System.Directory
import System.FilePath
import System.IO


-- * StateVars

infixr 2 &=, &=!

(&=) :: (MonadIO m, HasSetter s) => s a -> a -> m ()
v &= a = liftIO (v $= a)

(&=!) :: (MonadIO m, HasSetter s) => s a -> a -> m ()
v &=! a = liftIO (v $=! a)

the :: MonadIO m => HasGetter s => s a -> m a
the v = liftIO $ get v

xmap :: (b -> a) -> (a -> b) -> StateVar a -> StateVar b
xmap f g v = makeStateVar (g <$> get v) (\x -> v $= f x)

-- * Shaders and Programs

data ShaderException = ShaderException
  { shaderExceptionFileName :: String
  , shaderExceptionLog :: String
  } deriving (Show,Typeable,Generic)

instance Exception ShaderException

_ShaderException :: Prism' SomeException ShaderException
_ShaderException = exception

data ProgramException = ProgramException
  { programExceptionLog :: String
  } deriving (Show,Typeable)
instance Exception ProgramException

_ProgramException :: Prism' SomeException ProgramException
_ProgramException = exception

data ShaderEnv = ShaderEnv
  { _shaderEnvFragmentHighPrecisionAvailable   :: !Bool
  , _shaderEnvCpphsOpts :: CpphsOptions
  } deriving Typeable

makeClassy ''ShaderEnv

-- cpphs solves includes for us, but it eats the GL_PRECISION_HIGH define!

peek2 :: Storable a => Ptr a -> IO (a,a)
peek2 p = (,) <$> peekElemOff p 0 <*> peekElemOff p 1

-- TODO: when <https://github.com/haskell-opengl/OpenGL/issues/63> gets resolved, do this directly through the OpenGL package
fragmentHighp :: IO Bool
fragmentHighp = allocaArray 2 $ \p -> alloca $ \q -> do
  glGetShaderPrecisionFormat gl_FRAGMENT_SHADER gl_HIGH_FLOAT p q
  pq <- (,) <$> peek2 p <*> peek q
  return $ pq /= ((0,0),0)

defined :: HasShaderEnv s => Lens' s [(String,String)]
defined f = shaderEnvCpphsOpts go where
  go opts = f (defines opts) <&> \ds -> opts { defines = ds } 

buildShaderEnv :: Options -> IO ShaderEnv
buildShaderEnv opts = do
  highp <- fragmentHighp
  return $ ShaderEnv highp defaultCpphsOptions 
    { defines = concat 
      [ [ ("GL_FRAGMENT_PRECISION_HIGH","1") | highp ]
      , [ ("__VERSION__","410")
        , ("GL_core_profile","1")
        ]
      , defines defaultCpphsOptions
      ]
    , boolopts = boolOptions 
    , includes = [".",opts^.optionsDataDir]
    }

boolOptions :: BoolOptions
boolOptions = defaultBoolOptions 
  { macros    = True
  , locations = False -- #line directives in glsl have a different format, and #line must appear after #version anyways
  , hashline  = False
  , pragma    = True
  , stripEol  = True  -- strip comments
  , stripC89  = True
  , lang      = False -- C-like, not Haskell-like
  , ansi      = False -- disallow ## tricks for now
  , layout    = True  -- keep formatting
  , literate  = False -- no literate shaders please
  , warnings  = False -- we have to disable these or cpphs complains about #version
  }

readFirst :: [FilePath] -> FilePath -> IO (FilePath, String)
readFirst [] fn = do
  hPutStrLn stderr $ "missing file: " ++ fn
  fail $ "missing file: " ++ fn
readFirst (p:ps) fn = do
  let pfn = p </> fn
  ok <- doesFileExist pfn
  if ok then (,) pfn <$> readFile pfn
        else readFirst ps fn

-- | C preprocess a file
cpp :: (MonadIO m, MonadReader e m, HasShaderEnv e) => FilePath -> m String
cpp fp = do
  opts <- view shaderEnvCpphsOpts
  liftIO $ do
    (fp',content) <- readFirst (includes opts) fp
    runCpphs opts fp' content

-- | cpphs only copies through pragmas. write everything as a pragma.
hack :: String -> String
hack ('#':'p':'r':'a':'g':'m':'a':' ':xs) = '#':xs
hack xs = xs

-- | Compile a shader with a given set of defines
compile :: (MonadIO m, MonadReader e m, HasShaderEnv e) => ShaderType -> FilePath -> m Shader
compile st fp = do
  source <- liftM (unlines . map hack . lines) $ cpp fp
  liftIO $ do
    s <- createShader st
    shaderSourceBS s $= UTF8.fromString source
    compileShader s
    compiled <- the (compileStatus s)
    unless compiled $ do
      e <- the (shaderInfoLog s)
      -- putStrLn source
      deleteObjectName s
      let err = ShaderException fp e
      print err
      throw err
    return s
  
-- | Link a program and vertex shader to build a program
link :: MonadIO m => Shader -> Shader -> m Program
link vs fs = liftIO $ do
  p <- createProgram
  attachShader p vs
  attachShader p fs
  linkProgram p
  linked <- the $ linkStatus p
  unless linked $ do
    e <- the $ programInfoLog p
    deleteObjectName p
    throw $ ProgramException e
  return p

-- * Caching

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
