{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Tests for `Quine.GL.Buffer`
--------------------------------------------------------------------------------
module Main where

import Control.Exception.Base
import Control.Monad hiding (sequence)
import Control.Applicative
import Data.Bits
import Data.Proxy
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Default
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Internal (updPtr)
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.Var
import GHC.Generics
import Graphics.GL.Ext.EXT.DirectStateAccess
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import Graphics.UI.SDL as SDL
import Linear
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
import Quine.SDL
import Test.Hspec

--------------------------------------------------------------------------------
-- * Fixtures
--------------------------------------------------------------------------------

data AnAttribute f = AnAttribute
  { attrPosition :: f Vec3
  , attrNormal   :: f Vec3
  , attrTexture  :: f Vec2
  } deriving (Generic)
type VertexAttribute = AnAttribute UnAnnotated

deriving instance Show VertexAttribute
deriving instance Eq VertexAttribute

instance Storable VertexAttribute where
  sizeOf _ = 2 * sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2)
  alignment _ = alignment (undefined::Vec3) 
  peekByteOff p o = 
    AnAttribute <$> peekByteOff p o
                <*> peekByteOff p (o + sizeOf(undefined::Vec3))
                <*> peekByteOff p (o + sizeOf(undefined::Vec3) + sizeOf(undefined::Vec3))

  pokeByteOff p o AnAttribute{..} = do
    pokeByteOff p o attrPosition
    pokeByteOff p (o + sizeOf(attrPosition)) attrNormal
    pokeByteOff p (o + sizeOf(attrPosition) + sizeOf(attrNormal)) attrTexture

instance HasLayoutAnnotation AnAttribute where
  layoutAnnotation p = AnAttribute
    { attrPosition = LayoutAnnotation $ Layout (components $ attrPosition <$> p) (baseType $ attrPosition <$> p) False (sizeOfProxy p) nullPtr
    , attrNormal   = LayoutAnnotation $ Layout (components $ attrNormal   <$> p) (baseType $ attrNormal   <$> p) False (sizeOfProxy p) (nullPtr `plusPtr` sizeOfProxy (attrPosition <$> p))
    , attrTexture  = LayoutAnnotation $ Layout (components $ attrTexture  <$> p) (baseType $ attrTexture  <$> p) False (sizeOfProxy p) (nullPtr `plusPtr` sizeOfProxy (attrPosition <$> p) `plusPtr` sizeOfProxy (attrNormal <$> p))
    }

attributeInterleaved :: V.Vector (VertexAttribute)
attributeInterleaved = V.fromList
  [ mkAttribute (V3 (-1) 0 0) (V3 0 0 1) (V2 0   0)
  , mkAttribute (V3   0  1 0) (V3 0 0 1) (V2 0.5 0)
  , mkAttribute (V3   1 0 0)  (V3 0 0 1) (V2 1   0)
  ]

-- with annotations it is a bit clumsy to construct an attribute now (=RFC)
mkAttribute :: Vec3 -> Vec3 -> Vec2 -> VertexAttribute
mkAttribute p n t = AnAttribute (UnAnnotated p) (UnAnnotated n) (UnAnnotated t)


vertexSrc = BS.pack $ unlines 
  [ "#version 410"
  , "in vec3 aPosition;"
  , "in vec3 aNormal;"
  , "in vec2 aTexture;"
  , "void main(){aNormal;aTexture;gl_Position=vec4(aPosition, 1.0);}"
  ]

fragSrc = BS.pack $ unlines 
  [ "#version 410"
  , "out vec4 color;"
  , "void main(){color=vec4(1.0);}"
  ]

--------------------------------------------------------------------------------
-- * Setup
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- * Tests
--------------------------------------------------------------------------------

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

      it "should fail to download something from the 0-default buffer" $ do
        boundBufferAt ArrayBuffer $= def
        _re <- (get $ bufferData ArrayBuffer) :: IO (BufferUsage, V.Vector Int)
        errors >>= (`shouldSatisfy` (==[InvalidOperation]))

      it "should fail to download directly something for the 0-default buffer" $ do
        _re <- (get $ bufferDataDirect def) :: IO (BufferUsage, V.Vector Int)
        errors >>= (`shouldSatisfy` (==[InvalidOperation]))

    it "can be written interleaved (VNTVNTVNTVNT) to the buffer and linked to vertex shader attributes without an error (smoke test)" $ do

      vertShader <- createShaderFromSource GL_VERTEX_SHADER vertexSrc
      fragShader <- createShaderFromSource GL_FRAGMENT_SHADER fragSrc

  
      prog <- link [vertShader,fragShader]

      Just iPosition <- attributeLocation prog "aPosition"
      Just iNormal   <- attributeLocation prog "aNormal"
      Just iTexture  <- attributeLocation prog "aTexture"

      -- a vao is necessary because it "stores all of the state needed to supply vertex data" -- from the opengl wiki
      (boundVertexArray $=) =<< gen
      
      (boundBufferAt ArrayBuffer $=) =<< (gen :: IO (Buffer (V.Vector VertexAttribute)))
      bufferData ArrayBuffer $= (StaticDraw, attributeInterleaved)
      
      vertexAttribute iPosition $= Just attrPosition
      vertexAttribute iNormal   $= Just attrNormal
      vertexAttribute iTexture  $= Just attrTexture

      errors >>= (`shouldSatisfy` null)
      (get (bufferData ArrayBuffer)) `shouldReturn` (StaticDraw, attributeInterleaved)
      errors >>= (`shouldSatisfy` null)


createShaderFromSource shaderType src = do
  s <- createShader shaderType
  shaderSource s $= src
  compileShader s    
  compileStatus s `shouldReturn` True
  return s


sizeOfProxy :: forall a p. Storable a => p a -> Int
sizeOfProxy _ = sizeOf (undefined::a)
