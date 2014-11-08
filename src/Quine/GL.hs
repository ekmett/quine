{-# LANGUAGE DeriveDataTypeable #-}
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
    ShaderEnv(..)
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
  ) where

import Control.Applicative
import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Data
import Data.Foldable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core41
import Language.Preprocessor.Cpphs
import Prelude hiding (concat)
import Quine.GL.Shader
import Quine.GL.Object
import Quine.GL.Program
import Quine.Options
import Quine.StateVar
import System.Directory
import System.FilePath
import System.IO

-- * Shaders and Programs

data ShaderException = ShaderException
  { shaderExceptionFileName :: String
  , shaderExceptionLog :: Strict.ByteString
  } deriving (Show,Typeable)

instance Exception ShaderException

_ShaderException :: Prism' SomeException ShaderException
_ShaderException = exception

data ProgramException = ProgramException
  { programExceptionLog :: Strict.ByteString
  } deriving (Show,Typeable)
instance Exception ProgramException

_ProgramException :: Prism' SomeException ProgramException
_ProgramException = exception

data ShaderEnv = ShaderEnv
  { _shaderEnvFragmentHighPrecisionAvailable   :: !Bool
  , _shaderEnvCpphsOpts :: CpphsOptions
  }

makeClassy ''ShaderEnv

-- cpphs solves includes for us, but it eats the GL_PRECISION_HIGH define!

peek2 :: Storable a => Ptr a -> IO (a,a)
peek2 p = (,) <$> peekElemOff p 0 <*> peekElemOff p 1

-- TODO: when <https://github.com/haskell-opengl/OpenGL/issues/63> gets resolved, do this directly through the OpenGL package
fragmentHighp :: IO Bool
fragmentHighp = allocaArray 2 $ \p -> alloca $ \q -> do
  glGetShaderPrecisionFormat GL_FRAGMENT_SHADER GL_HIGH_FLOAT p q
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
    shaderSource s $= UTF8.fromString source
    compileShader s
    compiled <- compileStatus s
    unless compiled $ do
      e <- shaderInfoLog s
      delete s
      throw $ ShaderException fp e
    return s
  
-- | Link a program and vertex shader to build a program
link :: MonadIO m => Shader -> Shader -> m Program
link vs fs = liftIO $ do
  p <- gen
  attachShader p vs
  attachShader p fs
  linkProgram p
  linked <- linkStatus p
  unless linked $ do
    e <- programInfoLog p
    delete p
    throw $ ProgramException e
  return p
