{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.StateVar
import Foreign
import Foreign.C
import GHC.Conc
import Graphics.GL.Core41
import Graphics.UI.SDL as SDL
import Linear
import Numeric (showFFloat)
import Options.Applicative
import Prelude hiding (init)
import Quine.Camera
import Quine.Debug
import Quine.Display
import Quine.Env
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
import Quine.System
import System.Exit
import System.FilePath
import System.IO

#include "locations.h"

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
  contextMajorVersion $= 3
  contextMinorVersion $= 3
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
translate v = identity & translation .~ v

core :: (MonadIO m, MonadState s m, HasSystem s (), MonadReader e m, HasEnv e, HasOptions e) => m a
core = do
  liftIO (getDir "shaders") >>= \ ss -> buildNamedStrings ss ("/shaders"</>)
  screenShader <- compile ["/shaders"] GL_VERTEX_SHADER   "shaders/screen.vert"
  sceneShader  <- compile ["/shaders"] GL_FRAGMENT_SHADER =<< view optionsFragment
  scn <- link [screenShader,sceneShader]
  emptyVAO <- gen
  iResolution        <- programUniform2f scn `liftM` uniformLocation scn "iResolution"
  iGlobalTime        <- (mapStateVar realToFrac realToFrac . programUniform1f scn) `liftM` uniformLocation scn "iGlobalTime"
  iPhysicsAlpha      <- (mapStateVar realToFrac realToFrac . programUniform1f scn) `liftM` uniformLocation scn "iPhysicsAlpha"
  iCamera            <- programUniform3f scn `liftM` uniformLocation scn "iCamera" -- yaw, pitch, fovy
  iProjection        <- (SettableStateVar . uniformMat4) `liftM` uniformLocation scn "iProjection"
  iInverseProjection <- (SettableStateVar . uniformMat4) `liftM` uniformLocation scn "iProjection"
  iView              <- (SettableStateVar . uniformMat4) `liftM` uniformLocation scn "iView"
  iInverseView       <- (SettableStateVar . uniformMat4) `liftM` uniformLocation scn "iInverseView"
  throwErrors
  currentProgram   $= scn
  boundVertexArray $= emptyVAO
  forever $ do 
    (alpha,t) <- simulate $ poll $ \e -> handleDisplayEvent e >> handleInputEvent e
    displayMeter %= tick t
    
    displayFPS <- uses displayMeter fps 
    physicsFPS <- uses simulationMeter fps
    let title = showString "quine (display " 
            $ showFFloat (Just 1) displayFPS
            $ showString " fps, physics "
            $ showFFloat (Just 1) physicsFPS ")"
    use displayWindow >>= liftIO . withCString title . setWindowTitle

    updateCamera
    resizeDisplay 
    render $ do
      (w,h) <- use displayWindowSize
      let wf = fromIntegral w
          hf = fromIntegral h
      c <- use camera
      let cameraQuat = axisAngle (V3 1 0 0) (c^.pitch) * axisAngle (V3 0 1 0) (c^.yaw)
      let inverseCameraQuat = axisAngle (V3 0 1 0) (-c^.yaw) * axisAngle (V3 1 0 0) (-c^.pitch)
      iResolution        $= V2 wf hf
      iProjection        $= perspective (c^.fovy) (wf/hf) (c^.nearZ) (c^.farZ)
      iInverseProjection $= inversePerspective (c^.fovy) (wf/hf) (c^.nearZ) (c^.farZ)
      iView              $= m33_to_m44 (fromQuaternion cameraQuat)
      iInverseView       $= m33_to_m44 (fromQuaternion inverseCameraQuat)
      iCamera            $= V3 (c^.yaw) (c^.pitch) (c^.fovy)
      iGlobalTime        $= t
      iPhysicsAlpha      $= alpha
      glDrawArrays GL_TRIANGLES 0 3

render :: (MonadIO m, MonadReader e m, HasEnv e, MonadState s m, HasDisplay s) => m () -> m ()
render kernel = do
  inc =<< view (env.frameCounter)
  glClearColor 0 0 0 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  kernel
  w <- use displayWindow
  liftIO $ glSwapWindow w
