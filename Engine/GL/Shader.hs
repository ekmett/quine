-- | Shader support
--
-- TODO: consider supporting binary shader formats for faster startup
module Engine.GL.Shader 
  ( compile
  , link
  , Shader
  ) where

import Control.Monad
import Data.ByteString.UTF8 as UTF8
import Data.Functor
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core31
import Language.Preprocessor.Cpphs
import Paths_engine

import Unsafe.Coerce

-- damn it
shaderId :: Shader -> GLuint
shaderId = unsafeCoerce

programId :: Program -> GLuint
programId = unsafeCoerce

opts :: CpphsOptions
opts = defaultCpphsOptions 
  { boolopts = defaultBoolOptions 
    { macros = False
    , locations = True
    , hashline = True
    , pragma = True
    , stripEol = True
    , stripC89 = True
    , lang = False
    , ansi = False
    , layout = True
    , literate = False
    , warnings = True
    }
  }

cpphs :: [(String,String)] -> FilePath -> IO String
cpphs ds fp = do
  fp' <- getDataFileName fp
  content <- readFile fp'
  dataDir <- getDataDir
  runCpphs opts { defines = ds, includes = [dataDir, "data"] } fp' content

-- | Compile a shader with a given set of defines
compile :: ShaderType -> [(String,String)] -> FilePath -> IO Shader
compile st ds fp = do
  source <- UTF8.fromString <$> cpphs ds fp
  s <- createShader st
  shaderSourceBS s $= source
  compileShader s
  ok <- get (compileStatus s)
  unless ok $ do
    e <- get (shaderInfoLog s)
    glDeleteShader (shaderId s)
    fail e
  return s
  
-- | Link a program and vertex shader to build a program
link :: Shader -> Shader -> IO Program
link vs fs = do
  p <- createProgram
  attachShader p vs
  attachShader p fs
  linkProgram p
  linked <- get $ linkStatus p
  unless linked $ do
    e <- get (programInfoLog p)
    glDeleteProgram (programId p)
    fail e
  return p
