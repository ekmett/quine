{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Exception.Lens
import Control.Lens hiding (assign)
import Control.Lens.Extras (is)
import Control.Monad hiding (forM_)
import Control.Monad.Reader
import Control.Monad.State hiding (get)
import Data.Default
import Data.FileEmbed
import Data.Monoid
import Foreign
import Foreign.C
import GHC.Conc
import System.Exit
import System.FilePath
import System.IO
import Graphics.GL.Core41
import Graphics.UI.SDL as SDL
import Linear
import Numeric (showFFloat)
import Options.Applicative
import Prelude hiding (init)
import Quine.Camera
import Quine.Env
import Quine.Debug
import Quine.Display
import Quine.Exception
import Quine.GL
import Quine.GL.Error
import Quine.GL.Object
import Quine.GL.Program
import Quine.GL.Shader
import Quine.GL.Types
import Quine.GL.Uniform
import Quine.GL.Version as GL
import Quine.GL.VertexArray
import Quine.Input
import Quine.Meter
import Quine.Monitor
import Quine.Options
import Quine.SDL as SDL
import Quine.Simulation
import Quine.StateVar
import Quine.System

-- | I need to switch to UBOs!
data UniformCamera = UniformCamera
  { _uniformProjection
  , _uniformModelView :: SettableStateVar Mat4
  , _uniformFovy
  , _uniformAspectRatio
  , _uniformNear
  , _uniformFar :: StateVar Float
  }

makeClassy ''UniformCamera

programUniformCamera :: MonadIO m => Program -> Int -> m UniformCamera
programUniformCamera p s = liftIO $ do
  pro <- uniformLocation p $ "viewportCameraProjection[" ++ show s ++ "]"
  mv  <- uniformLocation p $ "viewportCameraModelView[" ++ show s ++ "]"
  fov <- uniformLocation p $ "viewportCameraFovy[" ++ show s ++ "]"
  a   <- uniformLocation p $ "viewportCameraAspectRatio[" ++ show s ++ "]"
  n   <- uniformLocation p $ "viewportCameraNear[" ++ show s ++ "]"
  f   <- uniformLocation p $ "viewportCameraFar[" ++ show s ++ "]"
  liftIO $ print pro
  return $ UniformCamera 
      (SettableStateVar (uniformMat4 pro)) -- TODO programUniformMat4
      (SettableStateVar (uniformMat4 mv))
      (programUniform1f p fov)
      (programUniform1f p a)
      (programUniform1f p n)
      (programUniform1f p f)

-- * State

main :: IO ()
main = runInBoundThread $ withCString "quine" $ \windowName -> do
  -- parse options
  opts <- execParser $ info (helper <*> parseOptions) $
    fullDesc
    <> progDesc "quine"
    <> header "Quine"

  -- be careful with exceptions
  setUncaughtExceptionHandler $ \ e -> if
    | is _Shutdown e -> return ()
    | otherwise -> do
      hPrint stderr e
      hFlush stderr
      exitFailure

  -- set up EKG
  ekg <- forkMonitor opts

  label "sdl.version" ekg >>= ($= show SDL.version)
 
  -- start SDL
  init SDL_INIT_EVERYTHING >>= err
  contextMajorVersion $= 4
  contextMinorVersion $= 1
  contextProfileMask  $= SDL_GL_CONTEXT_PROFILE_CORE
  redSize   $= 5
  greenSize $= 5
  blueSize  $= 5
  depthSize $= 16
  doubleBuffer $= True
  let w = opts^.optionsWindowWidth
      h = opts^.optionsWindowHeight
      flags = SDL_WINDOW_OPENGL
          .|. SDL_WINDOW_SHOWN
          .|. SDL_WINDOW_RESIZABLE
          .|. (if opts^.optionsHighDPI then SDL_WINDOW_ALLOW_HIGHDPI else 0)
          .|. (if | not (opts^.optionsFullScreen) -> 0
                  | opts^.optionsFullScreenNormal -> SDL_WINDOW_FULLSCREEN
                  | otherwise                     -> SDL_WINDOW_FULLSCREEN_DESKTOP)
  window <- createWindow windowName SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED (fromIntegral w) (fromIntegral h) flags >>= errOnNull

  -- start OpenGL
  cxt <- glCreateContext window >>= errOnNull

  makeCurrent window cxt

  _ <- glSetSwapInterval 0 -- turn of sync at least on everything but OSX.

  when (opts^.optionsDebug) installDebugHook

  label "gl.vendor" ekg           >>= ($= vendor)
  label "gl.renderer" ekg         >>= ($= renderer)
  label "gl.version" ekg          >>= ($= show GL.version)
  label "gl.shading.version" ekg  >>= ($= show shadingLanguageVersion)
  label "gl.shading.versions" ekg >>= ($= show shadingLanguageVersions)
  
  -- glEnable gl_FRAMEBUFFER_SRGB

  throwErrors
  fc <- counter "quine.frame" ekg
  vw <- gauge "viewport.width" ekg
  vh <- gauge "viewport.height" ekg
  let sys = Env ekg opts fc vw vh
      dsp = Display 
        { _displayWindow            = window
        , _displayGL                = cxt
        , _displayFullScreen        = opts^.optionsFullScreen
        , _displayWindowSize        = (fromIntegral w, fromIntegral h)
        , _displayWindowSizeChanged = True
        , _displayMinimized         = False
        , _displayHasMouseFocus     = True
        , _displayHasKeyboardFocus  = True
        , _displayVisible           = True
        , _displayMeter             = def
        }
  relativeMouseMode $= True -- switch to relative mouse mouse initially
  sim <- createSimulation ekg () ()
  handling id print (runReaderT (evalStateT core $ System dsp def def sim) sys) `finally` do
    glDeleteContext cxt
    destroyWindow window
    quit
    exitSuccess

translate :: Vec3 -> Mat4
translate v = eye4 & translation .~ v

core :: (MonadIO m, MonadState s m, HasSystem s (), MonadReader e m, HasEnv e, HasOptions e) => m a
core = do
  liftIO (getDir "shaders") >>= \ ss -> buildNamedStrings ss ("/shaders"</>)
  throwErrors
  liftIO $ putStrLn "compiling"
  viewportsView <- compile GL_VERTEX_SHADER   "shaders/viewports.vert"
  viewportsGeom <- compile GL_GEOMETRY_SHADER "shaders/viewports.geom"
  raymarchFrag  <- compile GL_FRAGMENT_SHADER "shaders/raymarch.frag"
  scene <- link [viewportsView,viewportsGeom,raymarchFrag]
  currentProgram $= scene
  emptyVAO <- gen
  throwErrors
  liftIO $ putStrLn "setting viewportCount"
  viewportCount <- programUniform1i scene `liftM` uniformLocation scene "viewportCount"
  viewportCount $= 1
  throwErrors
  liftIO $ putStrLn "retrieving camera"
  uc <- programUniformCamera scene 0
  uniformTime         <- (mapStateVar realToFrac realToFrac . programUniform1f scene) `liftM` uniformLocation scene "time"
  uniformPhysicsAlpha <- (mapStateVar realToFrac realToFrac . programUniform1f scene) `liftM` uniformLocation scene "physicsAlpha"
  throwErrors
  boundVertexArray $= emptyVAO
  liftIO $ putStrLn "setting up program"
  forever $ do 
    
    (alpha,t) <- simulate $ poll $ \e -> handleDisplayEvent e >> handleInputEvent e
    uniformTime         $= t
    uniformPhysicsAlpha $= alpha
    displayFPS <- uses displayMeter fps 
    physicsFPS <- uses simulationMeter fps
    displayMeter        %= tick t
    let title = showString "quine (display " 
              $ showFFloat (Just 1) displayFPS
              $ showString " fps, physics "
              $ showFFloat (Just 1) physicsFPS ")"
    use displayWindow >>= liftIO . withCString title . setWindowTitle
    resizeDisplay 
    updateCamera
    render $ do
      aspectRatio <- uses displayWindowSize $ \ (w,h) -> fromIntegral w / fromIntegral h
      c <- use camera
      uc^.uniformProjection  $= perspective (c^.fovy) aspectRatio (c^.nearZ) (c^.farZ)
      let cameraQuat = axisAngle (V3 1 0 0) (c^.pitch) * axisAngle (V3 0 1 0) (c^.yaw)
      let mv = set translation (V3 1 0 0) eye4 !*! m33_to_m44 (fromQuaternion cameraQuat)
      liftIO $ print mv
      uc^.uniformModelView   $= mv
      uc^.uniformFovy        $= c^.fovy
      uc^.uniformAspectRatio $= aspectRatio
      uc^.uniformNear        $= c^.nearZ
      uc^.uniformFar         $= c^.farZ
      glDrawArrays GL_POINTS 0 1

render :: (MonadIO m, MonadReader e m, HasEnv e, MonadState s m, HasDisplay s) => m () -> m ()
render kernel = do
  inc =<< view (env.frameCounter)
  glClearColor 1 0 0 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  kernel
  glFlush
  w <- use displayWindow
  liftIO $ glSwapWindow w
