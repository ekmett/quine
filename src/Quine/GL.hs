{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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
  -- * Compiling and Linking
    compile
  , link
  -- * Exceptions
  , ProgramException(..), _ProgramException
  , ShaderException(..), _ShaderException
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Data
import Prelude hiding (concat)
import Quine.GL.Shader
import Quine.GL.Object
import Quine.GL.Program
import Quine.StateVar

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

-- | Compile a shader with @#include@ support if it is available.
compile :: MonadIO m => ShaderType -> FilePath -> m Shader
compile st fp = do
  source <- liftIO $ readFile fp
  s      <- createShader st
  shaderSource s $= UTF8.fromString source
  compileShaderInclude s ["/includes"]
  compiled <- compileStatus s
  unless compiled $ do
    e <- shaderInfoLog s
    delete s
    liftIO $ print s
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
