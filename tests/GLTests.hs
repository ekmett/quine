module Main where

import Prelude hiding (sequence)
import Test.Hspec
import Control.Exception.Base
import Control.Monad hiding (sequence)
import Graphics.UI.GLFW as GLFW
import Data.Traversable

import Quine.GL.Buffer
import Quine.GL.Object
import Quine.GL.Error
import Quine.StateVar


gl45Test =
  [ WindowHint'Visible False
  , WindowHint'ClientAPI ClientAPI'OpenGL
  , WindowHint'ContextVersionMajor 4
  , WindowHint'ContextVersionMinor 1 -- currently max on os x
  , WindowHint'OpenGLForwardCompat  True
  , WindowHint'OpenGLProfile OpenGLProfile'Core
  , WindowHint'OpenGLDebugContext False
  ]


withGLContext :: [WindowHint] -> IO a -> IO a
withGLContext settings action = do
  bracket
    (do
      bInited <- GLFW.init
      unless bInited $ error "GLFW not initialized"
      traverse windowHint settings
      Just win <- createWindow 10 10 "hspec" Nothing Nothing
      makeContextCurrent $ Just win
      return win
    )
   (\win -> destroyWindow win >> terminate)
   (const action)

main = hspec $ around_ (withGLContext gl45Test) $ do
  describe "Buffer generation" $ do
    it "at least some buffers generatable" $ do
      buffs <- gens 1024
      length (buffs :: [Buffer ()]) `shouldBe` 1024
      throwErrors


  context "A name returned by glGenBuffers, but not yet associated with a buffer object by calling glBindBuffer, is not the name of a buffer object." $ do
    it "is generated but unbound" $ do
      buff <- gen
      isa (buff :: Buffer ()) `shouldReturn` False
      throwErrors

    it "is generated and bound and now a valid buffer" $ do
      buff <- gen
      boundBufferAt ArrayBuffer $= buff
      isa (buff :: Buffer ()) `shouldReturn` True
      throwErrors

  context "upload data to buffer" $ do
    it "can buffer a simple haskell list" $ do
      buff <- gen :: IO (Buffer [Int])
      boundBufferAt ArrayBuffer $= buff
      bufferData ArrayBuffer $= (StaticDraw, [1, 2, 3, 4] :: [Int])
      errors >>= (`shouldSatisfy` null)
