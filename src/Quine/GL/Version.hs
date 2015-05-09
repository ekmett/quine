{-# LANGUAGE CPP #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.GL.Version
  ( vendor
  , renderer
  , version
  , shadingLanguageVersion
  , shadingLanguageVersions
  , gles
  ) where

import Control.Monad
#if ! MIN_VERSION_base(4,8,0)
import Data.Functor
#endif
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Set as Set
import Data.Version
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core43
import Graphics.GL.Types
import System.IO.Unsafe
import Text.ParserCombinators.ReadP

-- |
-- @
-- major.minor <vendor-specific information>
-- major.minor.release <vendor-specific information>
-- OpenGL ES major.minor <vendor-specific information>
-- OpenGL ES major.minor.release <vendor-specific information>
-- OpenGL ES GLSL ES major.minor <vendor-specific information>
-- OpenGL ES GLSL ES major.minor.release <vendor-specific information>
-- @
parse :: String -> Maybe Version
parse s = case words s of
  "OpenGL":"ES":"GLSL":"ES":x:_ -> go x
  "OpenGL":"ES":x:_             -> go x
  x:_                           -> go x
  _ -> Nothing
  where go xs = listToMaybe [ v | (v,"") <- readP_to_S parseVersion xs ]

getString :: GLenum -> IO String
getString = glGetString >=> peekCString . castPtr

-- | Returns the company responsible for this GL implementation. This name does not change from release to release.
vendor :: String
vendor = unsafePerformIO $ getString GL_VENDOR
{-# NOINLINE vendor #-}

-- | Returns the name of the renderer. This name is typically specific to a particular configuration of a hardware platform. It does not change from release to release.
renderer :: String
renderer = unsafePerformIO $ getString GL_RENDERER
{-# NOINLINE renderer #-}

versionString :: String
versionString = unsafePerformIO $ getString GL_VERSION
{-# NOINLINE versionString #-}

gles :: Bool
gles = "OpenGL ES" `isPrefixOf` versionString

-- | Returns a version or release number.
version :: Version
version = error ("malformed GL version: " ++ versionString) `fromMaybe` parse versionString

shadingLanguageVersionString :: String
shadingLanguageVersionString = unsafePerformIO $ getString GL_SHADING_LANGUAGE_VERSION
{-# NOINLINE shadingLanguageVersionString #-}

-- | Return the primary shading language version
shadingLanguageVersion :: Version
shadingLanguageVersion = Version [] [] `fromMaybe` parse shadingLanguageVersionString

-- | Returns a set of shading language versions supported by this implementation.
shadingLanguageVersions :: Set Version
shadingLanguageVersions 
  | version >= Version [4,3] [] = unsafePerformIO $ do
    n <- alloca $ \p -> do
      poke p 0 -- unsupported until 4.2, so scribble a 0 in first
      glGetIntegerv GL_NUM_SHADING_LANGUAGE_VERSIONS p
      peek p
    versions <- forM [0..fromIntegral n-1] $ \i -> do
      cs <- glGetStringi GL_SHADING_LANGUAGE_VERSION i
      parse <$> peekCString (castPtr cs)
    return $ Set.fromList $ catMaybes versions
  | otherwise = Set.singleton shadingLanguageVersion

{-# NOINLINE shadingLanguageVersions #-}
