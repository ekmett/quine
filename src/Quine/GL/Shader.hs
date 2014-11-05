{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, LambdaCase, PatternSynonyms #-}
module Quine.GL.Shader
  ( 
  -- * Shader types
    ShaderType(..)
  , pattern ComputeShader
  , pattern VertexShader
  , pattern FragmentShader
  , pattern GeometryShader
  , pattern TessControlShader
  , pattern TessEvaluationShader
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
  -- * OpenGL 4.1+, OpenGL ES 2+
  , releaseShaderCompiler
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Data
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Graphics.GL.Raw.Types
import Graphics.GL.Raw.Profile.Core45
import Quine.StateVar
import Quine.GL.Object

-- * Shader types

newtype ShaderType = ShaderType GLenum
  deriving (Eq,Ord,Typeable,Data,Generic)

pattern VertexShader         = ShaderType GL_VERTEX_SHADER
pattern TessControlShader    = ShaderType GL_TESS_CONTROL_SHADER
pattern TessEvaluationShader = ShaderType GL_TESS_EVALUATION_SHADER
pattern GeometryShader       = ShaderType GL_GEOMETRY_SHADER
pattern FragmentShader       = ShaderType GL_FRAGMENT_SHADER
pattern ComputeShader        = ShaderType GL_COMPUTE_SHADER

instance Show ShaderType where
  showsPrec d (ShaderType t) = case t of
    GL_VERTEX_SHADER          -> showString "VertexShader"
    GL_TESS_CONTROL_SHADER    -> showString "TessControlShader"
    GL_TESS_EVALUATION_SHADER -> showString "TessEvaluationShader"
    GL_GEOMETRY_SHADER        -> showString "GeometryShader"
    GL_FRAGMENT_SHADER        -> showString "FragmentShader"
    GL_COMPUTE_SHADER         -> showString "ComputeShader"
    _ -> showParen (d >= 10) $ showString "ShaderType " . showsPrec 11 t

-- * Shaders

newtype Shader = Shader GLuint deriving
  (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object Shader where
  object (Shader s) = s
  isa (Shader s) = (GL_FALSE /=) `liftM` glIsShader s 
  delete (Shader s) = glDeleteShader s

-- | Create a shader
createShader :: MonadIO m => ShaderType -> m Shader
createShader (ShaderType t) = Shader `liftM` glCreateShader t

-- | Compile a shader
compileShader :: MonadIO m => Shader -> m ()
compileShader (Shader s) = glCompileShader s

-- | Available on OpenGL 4.1+, OpenGL ES 2+
releaseShaderCompiler :: MonadIO m => m ()
releaseShaderCompiler = glReleaseShaderCompiler

getShader :: MonadIO m => Shader -> GLenum -> m GLint
getShader (Shader s) p = liftIO $ alloca $ \q -> glGetShaderiv s p q >> peek q

-- | Ask OpenGL for the 'ShaderType'
shaderType :: MonadIO m => Shader -> m ShaderType
shaderType s = liftM (ShaderType . fromIntegral) $ getShader s GL_SHADER_TYPE

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
        glGetShaderInfoLog (objectId s) (fromIntegral l') pl (castPtr ps)
        return $ l-1
