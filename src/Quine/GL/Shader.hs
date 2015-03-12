{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, LambdaCase, PatternSynonyms, BangPatterns #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.GL.Shader
  ( 
  -- * Shader types
    ShaderType
  , showShaderType
  -- * Shaders
  , Shader(..)
  , createShader
  , compileShader
  , shaderType
  , shaderIsDeleted
  , compileStatus
  , shaderSourceLength
  , shaderSource
  , shaderInfoLog
  -- * GL_ARB_shading_language_include
  , buildNamedStrings
  , compileShaderInclude
  -- * OpenGL 4.1+, OpenGL ES 2+
  , releaseShaderCompiler
  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Unsafe as Strict
import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Data
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.StateVar
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Ext.ARB.ShadingLanguageInclude
import Graphics.GL.Types
import Quine.GL.Object

-- * Shader types

type ShaderType = GLenum

showShaderType :: Int -> ShaderType -> ShowS
showShaderType d = \ case
  GL_VERTEX_SHADER          -> showString "GL_VERTEX_SHADER"
  GL_TESS_CONTROL_SHADER    -> showString "GL_TESS_CONTROL_SHADER"
  GL_TESS_EVALUATION_SHADER -> showString "GL_TESS_EVALUATION_SHADER"
  GL_GEOMETRY_SHADER        -> showString "GL_GEOMETRY_SHADER"
  GL_FRAGMENT_SHADER        -> showString "GL_FRAGMENT_SHADER"
  GL_COMPUTE_SHADER         -> showString "GL_COMPUTE_SHADER"
  t -> showsPrec d t

-- * Shaders

newtype Shader = Shader GLuint deriving
  (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object Shader where
  object (Shader s) = s
  isa (Shader s) = (GL_FALSE /=) `liftM` glIsShader s 
  delete (Shader s) = glDeleteShader s

-- | Create a shader
createShader :: MonadIO m => ShaderType -> m Shader
createShader = liftM Shader . glCreateShader

-- | Compile a shader
compileShader :: MonadIO m => Shader -> m ()
compileShader (Shader s) = glCompileShader s

withCStrings :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStrings all_xs f = go 0 [] all_xs where
  go !n acc (x:xs) = withCString x $ \s -> go (n + 1) (s:acc) xs
  go !n acc [] = allocaArray n $ \p -> do
    pokeArray p (reverse acc)
    f n p

-- | Available on OpenGL 4.1+, OpenGL ES 2+
releaseShaderCompiler :: MonadIO m => m ()
releaseShaderCompiler = glReleaseShaderCompiler

getShader :: MonadIO m => Shader -> GLenum -> m GLint
getShader (Shader s) p = liftIO $ alloca $ \q -> glGetShaderiv s p q >> peek q

-- | Ask OpenGL for the 'ShaderType'
shaderType :: MonadIO m => Shader -> m ShaderType
shaderType s = fromIntegral `liftM` getShader s GL_SHADER_TYPE

-- | Check if the shader is deleted in OpenGL
shaderIsDeleted :: MonadIO m => Shader -> m Bool
shaderIsDeleted s = (GL_TRUE ==) `liftM` getShader s GL_DELETE_STATUS

-- | Returns true if the last compileShader action on this shader completed successfully.
compileStatus :: MonadIO m => Shader -> m Bool
compileStatus s = (GL_TRUE ==) `liftM` getShader s GL_COMPILE_STATUS

-- | Retrieve the length of the source code of this shader, including the null terminating character
shaderSourceLength :: MonadIO m => Shader -> m Int
shaderSourceLength s = fromIntegral `liftM` getShader s GL_SHADER_SOURCE_LENGTH

shaderInfoLogLength :: MonadIO m => Shader -> m Int
shaderInfoLogLength s = fromIntegral `liftM` getShader s GL_INFO_LOG_LENGTH

-- | Get/set the source code to a shader.
shaderSource :: Shader -> StateVar Lazy.ByteString
shaderSource (Shader sh) = StateVar g s where
  g :: IO Lazy.ByteString
  g = alloca $ \pl -> do
    l <- glGetShaderiv sh GL_SHADER_SOURCE_LENGTH pl >> peek pl
    let l' = fromIntegral l
    if l <= 1
      then return Lazy.empty
      else do
        chunk <- Strict.createUptoN l' $ \ps -> do
          glGetShaderSource sh (fromIntegral l) pl (castPtr ps)
          return (l'-1)
        return $ Lazy.fromChunks [chunk]

  s :: Lazy.ByteString -> IO ()
  s bs = allocaArray (length chunks) $ \ps -> allocaArray (length chunks) $ \pl -> go 0 chunks ps pl
    where chunks = Lazy.toChunks bs

  go :: Int -> [Strict.ByteString] -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()
  go i (Strict.PS fp o l:cs) ps pl = do
    pokeElemOff pl i (fromIntegral l)
    withForeignPtr fp $ \p -> do
      pokeElemOff ps i (castPtr p `plusPtr` o)
      go (i+1) cs ps pl

  go i [] ps pl = glShaderSource sh (fromIntegral i) ps pl 

shaderInfoLog :: MonadIO m => Shader -> m Strict.ByteString
shaderInfoLog s = do
  l <- shaderInfoLogLength s
  let l' = fromIntegral l
  if l <= 1
    then return Strict.empty
    else liftIO $ alloca $ \pl -> do
      Strict.createUptoN l' $ \ps -> do
        glGetShaderInfoLog (object s) (fromIntegral l') pl (castPtr ps)
        return $ l-1

-- | 
--
-- Using statically embedded strings in the executable:
-- 
-- @
-- buildNamedStrings $(embedDir "foo") ('/':)
-- @
--
-- Dynamically embedding all files in a given directory into the named string set
--
-- @
-- getDir "foo" >>= \ ss -> buildNamedStrings ss ('/':)
-- @
--
-- Falls back to doing nothing if 'gl_ARB_shading_langauge_include' isn't available.
buildNamedStrings :: MonadIO m => [(FilePath, Strict.ByteString)] -> (FilePath -> String) -> m ()
buildNamedStrings includes tweak = liftIO $ when gl_ARB_shading_language_include $ do
  forM_ includes $ \(fp',body) -> do
    withCStringLen (tweak fp') $ \ (name, namelen) ->
      Strict.unsafeUseAsCString body $ \string -> do
        glNamedStringARB GL_SHADER_INCLUDE_ARB (fromIntegral namelen) name (fromIntegral $ Strict.length body) string

-- | Compile a shader with @#include@ support (if available).
--
-- Remember to use
--
-- @
-- #extension GL_ARB_shading_language_include : <behavior>
-- @
--
-- as appropriate within your shader.
--
-- Falls back to 'compileShader' if 'gl_ARB_shading_language_include' isn't available.
compileShaderInclude :: MonadIO m => Shader -> [FilePath] -> m ()
compileShaderInclude (Shader s) path
  | gl_ARB_shading_language_include = 
    liftIO $ withCStrings path $ \n cpcs -> glCompileShaderIncludeARB s (fromIntegral n) cpcs nullPtr
  | otherwise = glCompileShader s

