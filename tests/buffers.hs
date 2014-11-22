--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Tests for `Quine.GL.Buffer`
--------------------------------------------------------------------
module Main where

import Prelude hiding (sequence)
import Test.Hspec
import Control.Exception.Base
import Control.Monad hiding (sequence)
import Graphics.UI.GLFW as GLFW
import Data.Traversable
import Data.Coerce
import Data.Default
import qualified Data.Vector.Storable as V

import Quine.GL.Buffer
import Quine.GL.VertexArray
import Quine.GL.Object
import Quine.GL.Error
import Quine.StateVar

import Graphics.GL.Ext.EXT.DirectStateAccess
import Graphics.GL.Types
import Graphics.GL.Internal.Shared

gl41Test =
  [ WindowHint'Visible False
  , WindowHint'ClientAPI ClientAPI'OpenGL
  , WindowHint'ContextVersionMajor 4
  , WindowHint'ContextVersionMinor 1
  -- ^ 4.1 max on os x, 4.4 on windows with nvidia 344.75
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
      Just win <- createWindow 1 1 "hspec" Nothing Nothing
      makeContextCurrent $ Just win
      return win
    )
   (\win -> destroyWindow win >> terminate)
   (const action)

main = withGLContext gl41Test (evaluate gl_EXT_direct_state_access) >>= \dsa -> hspec $ around_ (withGLContext gl41Test) $ do
  -- * Buffer generation
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

  describe "Buffer upload" $ do
    context "indirect upload data to buffer" $ do

      -- * List uploads
      it "is possible to upload a simple haskell list" $ do
        buff <- gen :: IO (Buffer [Int])
        boundBufferAt ArrayBuffer $= buff
        bufferData ArrayBuffer $= (StaticDraw, [1, 2, 3, 5] :: [Int])
        errors >>= (`shouldSatisfy` null)

      -- * Vector uploads
      it "is possible to upload a Data.Vector.Storable" $ do
        buff <- gen :: IO (Buffer [Int])
        boundBufferAt ArrayBuffer $= buff
        bufferData ArrayBuffer $= (StaticDraw, V.fromList [1, 2, 3, 5] :: V.Vector Int)
        errors >>= (`shouldSatisfy` null)

      it "should fail to upload something to the 0-default buffer" $ do
        boundBufferAt ArrayBuffer $= def
        bufferData ArrayBuffer $= (StaticDraw, [1, 2, 3, 5] :: [Int])
        errors >>= (`shouldSatisfy` (==[InvalidOperation]))

    when dsa $ context "direct upload data to buffer" $ do
      it "is possible to direct upload" $ do
        vao <- gen :: IO VertexArray
        buff <- gen :: IO (Buffer [Int])

        -- inital setup to define buffer type
        boundBufferAt ArrayBuffer $= buff

        -- unbound default (0) buffer 
        boundBufferAt ArrayBuffer $= def

        -- works even without bound buffer
        bufferDataDirect buff $= (StaticDraw, [1, 2, 3, 5] :: [Int])
        errors >>= (`shouldSatisfy` null)

      it "should fail to upload directly something to the 0-default buffer" $ do
        bufferDataDirect def $= (StaticDraw, [1, 2, 3, 5] :: [Int])
        errors >>= (`shouldSatisfy` (==[InvalidOperation]))

  describe "Buffer download" $ do
    context "indirect buffer download data from buffer" $ do
      
      it "is possible to retrieve the same list data from a buffer" $ do
        let xs = [1, 2, 3, 5] :: [Int]
        
        buff <- gen :: IO (Buffer [Int])
        boundBufferAt ArrayBuffer $= buff
        bufferData ArrayBuffer $= (StaticDraw, xs)
        errors >>= (`shouldSatisfy` null)

        (get $ bufferData ArrayBuffer) `shouldReturn` (StaticDraw, xs)
        errors >>= (`shouldSatisfy` null)

      it "is possible to retrieve the same Vector data from a buffer" $ do
        let vec = V.fromList [1, 2, 3, 5] :: V.Vector Int
        
        buff <- gen :: IO (Buffer (V.Vector Int))
        boundBufferAt ArrayBuffer $= buff
        bufferData ArrayBuffer $= (StaticDraw, vec)
        errors >>= (`shouldSatisfy` null)

        (get $ bufferData ArrayBuffer) `shouldReturn` (StaticDraw, vec)
        errors >>= (`shouldSatisfy` null)    

    when dsa $ context "direct buffer download data from buffer" $ do
      
      it "is possible to retrieve the same list data from a buffer" $ do
        let xs = [1, 2, 3, 5] :: [Int]
        
        buff <- gen :: IO (Buffer [Int])
        boundBufferAt ArrayBuffer $= buff
        boundBufferAt ArrayBuffer $= def
        bufferDataDirect buff $= (StaticDraw, xs)
        errors >>= (`shouldSatisfy` null)

        (get $ bufferDataDirect buff) `shouldReturn` (StaticDraw, xs)
        errors >>= (`shouldSatisfy` null)

      it "is possible to retrieve the same Vector data from a buffer" $ do
        let vec = V.fromList [1, 2, 3, 5] :: V.Vector Int
        
        buff <- gen :: IO (Buffer (V.Vector Int))
        boundBufferAt ArrayBuffer $= buff
        boundBufferAt ArrayBuffer $= def
        bufferDataDirect buff $= (StaticDraw, vec)
        errors >>= (`shouldSatisfy` null)

        (get $ bufferDataDirect buff) `shouldReturn` (StaticDraw, vec)
        errors >>= (`shouldSatisfy` null)

      it "should fail to download something for the 0-default buffer" $ do
        boundBufferAt ArrayBuffer $= def
        re <- (get $ bufferData ArrayBuffer) :: IO (BufferUsage, V.Vector Int)
        errors >>= (`shouldSatisfy` (==[InvalidOperation]))

      it "should fail to download directly something for the 0-default buffer" $ do
        re <- (get $ bufferDataDirect def) :: IO (BufferUsage, V.Vector Int)
        errors >>= (`shouldSatisfy` (==[InvalidOperation]))

