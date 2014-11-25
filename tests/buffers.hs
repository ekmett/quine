{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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

import Test.Hspec
import Control.Exception.Base
import Control.Monad hiding (sequence)
import Data.Bits
import Data.Proxy
import qualified Data.ByteString.Lazy.Char8 as BS

import GHC.Generics
import Data.Default
import qualified Data.Vector.Storable as V
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

import Quine.GL.Buffer
import Quine.GL.Attribute
import Quine.GL.VertexArray
import Quine.GL.Object
import Quine.GL.Error
import Quine.GL.Types
import Quine.GL.Program
import Quine.GL.Uniform
import Quine.GL.Block
import Quine.GL.Shader
import Quine.GL
import Quine.StateVar
import Quine.SDL
import Graphics.UI.SDL as SDL
import Linear

import Graphics.GL.Ext.EXT.DirectStateAccess
import Graphics.GL.Internal.Shared
import Graphics.GL.Types

data VertexStream = VertexStream 
  { position :: [Vec3]
  , normal   :: [Vec3]
  , texture  :: [Vec2]
  } deriving (Generic)
vertexStream = VertexStream ([V3 (-1) 0 0, V3 0 1 0, V3 1 0 0])
                            ([V3 0 0 1   , V3 0 0 1, V3 0 0 1])
                            ([V2 0 0     , V2 0.5 1, V2 1 0])



vertexSrc = BS.pack $ unlines 
  [ "#version 410"
  , "in vec3 aPosition;"
  , "in vec3 aNormal;"
  -- , "in vec2 aTexture;"
  , "void main(){aNormal;gl_Position=vec4(aPosition, 1.0);}"
  ]

fragSrc = BS.pack $ unlines 
  [ "#version 410"
  , "out vec4 color;"
  , "void main(){color=vec4(1.0);}"
  ]

-- TODO remove me
instance Storable a => Storable [a] where
  sizeOf as = length as * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  poke ptr = pokeArray (castPtr ptr)

withGLContext :: IO a -> IO a
withGLContext action = do
  bracket
    (do
      SDL.init SDL_INIT_EVERYTHING >>= err
      contextMajorVersion $= 4
      contextMinorVersion $= 1
      contextProfileMask $= SDL_GL_CONTEXT_PROFILE_CORE
      win <- withCString "shaders" $ \windowName -> createWindow windowName SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED 1 1 (SDL_WINDOW_OPENGL .|. SDL_WINDOW_HIDDEN)
      cxt <- glCreateContext win
      makeCurrent win  cxt
      return (win, cxt)
    )
   (\(win, cxt) -> glDeleteContext cxt >> destroyWindow win >> quit)
   (const action)


main :: IO ()
main = withGLContext (evaluate gl_EXT_direct_state_access) >>= \dsa -> hspec $ around_ withGLContext $ do
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
        _vao <- gen :: IO VertexArray
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
        _re <- (get $ bufferData ArrayBuffer) :: IO (BufferUsage, V.Vector Int)
        errors >>= (`shouldSatisfy` (==[InvalidOperation]))

      it "should fail to download directly something for the 0-default buffer" $ do
        _re <- (get $ bufferDataDirect def) :: IO (BufferUsage, V.Vector Int)
        errors >>= (`shouldSatisfy` (==[InvalidOperation]))

  describe "Vertex Attributes" $ do

    it "(smoke test) are written to the buffer" $ do

      vertShader <- createShader GL_VERTEX_SHADER
      fragShader <- createShader GL_FRAGMENT_SHADER
      shaderSource vertShader $= vertexSrc
      shaderSource fragShader $= fragSrc
      compileShader vertShader
      compileShader fragShader
      compileStatus vertShader `shouldReturn` True
      compileStatus fragShader `shouldReturn` True
  
      prog <- link [vertShader,fragShader]

      iPosition <- attributeLocation prog "aPosition"
      iNormal   <- attributeLocation prog "aNormal"
      -- iTexture  <- attributeLocation prog "aTexture"
      posNormBuff <- gen :: IO (Buffer (STD140 VertexStream)) -- FIX type
      

      -- a vao is neccessary because it "stores all of the state needed to supply vertex data" -- from the opengl wiki
      (boundVertexArray $=) =<< gen
      -- in production: enable the vertex arrays, but it's not necessary for filling the buffer 
      -- glEnableVertexAttribArray (fromIntegral iPosition)
      -- glEnableVertexAttribArray (fromIntegral iNormal)

      boundBufferAt ArrayBuffer $= posNormBuff
      writeBufferData ArrayBuffer StaticDraw $ do
        assignAttribute iPosition AsFloating $ position vertexStream
        assignAttribute iNormal   AsFloating $ normal vertexStream

      errors >>= (`shouldSatisfy` null)
