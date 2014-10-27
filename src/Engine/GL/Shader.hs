{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Shader support
--
-- TODO: consider supporting binary shader formats for faster startup
module Engine.GL.Shader 
  ( ShaderEnv
  , HasShaderEnv(..)
  , buildShaderEnv
  , compile
  , link
  -- * Internal details
  , cpp
  , defined
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.ByteString.UTF8 as UTF8
import Data.Typeable
import Engine.Options
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.ARB.ES2Compatibility
import Graphics.Rendering.OpenGL.Raw.ARB.FragmentShader
import Language.Preprocessor.Cpphs
import System.Directory
import System.FilePath
import System.IO

data ShaderEnv = ShaderEnv
  { _shaderEnvFragmentHighPrecisionAvailable   :: !Bool
  , _shaderEnvCpphsOpts :: CpphsOptions
  } deriving Typeable

makeClassy ''ShaderEnv

-- cpphs solves includes for us, but it eats the GL_PRECISION_HIGH define!

peek2 :: Storable a => Ptr a -> IO (a,a)
peek2 p = (,) <$> peekElemOff p 0 <*> peekElemOff p 1

-- TODO: when <https://github.com/haskell-opengl/OpenGL/issues/63> gets resolved, do this directly through OpenGL
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
  b <- fragmentHighp
  return $ ShaderEnv b defaultCpphsOptions 
    { defines = if b then [("GL_FRAGMENT_PRECISION_HIGH","1")]  else []
    , boolopts = boolOptions 
    , includes = [opts^.optionsDataDir]
    }

boolOptions :: BoolOptions
boolOptions = defaultBoolOptions 
  { macros    = False -- don't leave #defines in
  , locations = False -- the #line directive eats our #version ?
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

-- | Compile a shader with a given set of defines
compile :: (MonadIO m, MonadReader e m, HasShaderEnv e) => ShaderType -> FilePath -> m Shader
compile st fp = do
  source <- cpp fp
  liftIO $ do
    s <- createShader st
    shaderSourceBS s $= UTF8.fromString source
    compileShader s
    compiled <- get (compileStatus s)
    unless compiled $ do
      e <- get (shaderInfoLog s)
      deleteObjectName s
      let msg = "error in shader: " ++ fp ++ "\n" ++ e
      hPutStrLn stderr msg
      hPutStrLn stderr "After CPP:"
      hPutStrLn stderr source
      fail msg
    return s
  
-- | Link a program and vertex shader to build a program
link :: MonadIO m => Shader -> Shader -> m Program
link vs fs = liftIO $ do
  p <- createProgram
  attachShader p vs
  attachShader p fs
  linkProgram p
  linked <- get $ linkStatus p
  unless linked $ do
    e <- get $ programInfoLog p
    deleteObjectName p
    fail e
  return p
