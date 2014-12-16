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
-- Tests for `Quine.GL.Framebuffer`
--------------------------------------------------------------------------------
module Main where

import Test.Hspec

import Control.Applicative
import Control.Exception.Base
import Control.Monad hiding (sequence)
import Data.Bits
import Data.Maybe
import Data.Proxy
import qualified Data.ByteString.Lazy.Char8 as BS

import GHC.Generics
import Data.Default
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Internal (updPtr)
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

import Graphics.UI.SDL as SDL hiding (Texture)
import Linear

import Quine.GL
import Quine.GL.Error
import Quine.GL.Framebuffer
import Quine.GL.Object
import Quine.GL.Texture
import Quine.SDL
import Quine.StateVar

import Graphics.GL.Internal.Shared
import Graphics.GL.Types


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
main = hspec $ around_ withGLContext $ do
  describe "Framebuffer Generation" $ do
    it "at least some framebuffers are generatable" $ do
      fbs <- gens 1024
      length (fbs :: [Framebuffer]) `shouldBe` 1024
      throwErrors

    context "binding of Framebuffers" $ do
      it "is not a Framebuffer if it is generated but never bound" $ do
        fb <- gen :: IO Framebuffer
        isa fb `shouldReturn` False
        throwErrors

      it "is a Framebuffer if it was bound at least once" $ do
        fb <- gen :: IO Framebuffer
        boundFramebuffer RWFramebuffer $= fb
        isa fb `shouldReturn` True
        throwErrors

  describe "Framebuffer Completeness" $ do
    it "the default Framebuffer should be complete" $ do
        boundFramebuffer RWFramebuffer $= def
        checkFramebufferStatus RWFramebuffer `shouldReturn` Nothing
        throwErrors
    
    -- it's 'GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT' and not 'GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT​' because
    -- there are no attachemnts to check for attachment completeness
    it "a fresh Framebuffer is an incomplete Framebuffer 'GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT'" $ do
      fb <- gen :: IO Framebuffer
      boundFramebuffer RWFramebuffer $= fb
      checkFramebufferStatus RWFramebuffer `shouldReturn` (Just $ FramebufferError GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT)
      throwErrors

    it "when one 'Texture' color attachment is incomplete the Framebuffer is also incomplete" $ do
      fb <- gen :: IO Framebuffer
      boundFramebuffer RWFramebuffer $= fb
      
      tex <- gen :: IO Texture
      boundTexture GL_TEXTURE_2D GL_TEXTURE_BINDING_2D $= tex
      attach RWFramebuffer GL_COLOR_ATTACHMENT0 tex
      
      checkFramebufferStatus RWFramebuffer `shouldReturn` (Just $ FramebufferError GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT)
      throwErrors

    it "a Framebuffer is complete with one complete 'Texture' color attachment" $ do
      fb <- gen :: IO Framebuffer
      boundFramebuffer RWFramebuffer $= fb
      
      tex <- gen :: IO Texture    
      boundTexture GL_TEXTURE_2D GL_TEXTURE_BINDING_2D $= tex
      glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 1 1 0 GL_BGRA GL_UNSIGNED_BYTE nullPtr
      attach RWFramebuffer GL_COLOR_ATTACHMENT0 tex
      
      checkFramebufferStatus RWFramebuffer `shouldReturn` Nothing
      throwErrors

    it "a Framebuffer is complete with one complete 'Texture' color attachment ('Texture' is unbound)" $ do
      fb <- gen :: IO Framebuffer
      boundFramebuffer RWFramebuffer $= fb
      
      tex <- gen :: IO Texture    
      boundTexture GL_TEXTURE_2D GL_TEXTURE_BINDING_2D $= tex
      glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 1 1 0 GL_BGRA GL_UNSIGNED_BYTE nullPtr
      boundTexture GL_TEXTURE_2D GL_TEXTURE_BINDING_2D $= def
      attach RWFramebuffer GL_COLOR_ATTACHMENT0 tex
      
      checkFramebufferStatus RWFramebuffer `shouldReturn` Nothing
      throwErrors

