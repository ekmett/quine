module Quine.GL.Version
  ( vendor
  , renderer
  , version
  , shadingLanguageVersion
  , shadingLanguageVersions
  ) where

import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Set as Set
import Data.Version
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Raw.Types
import Graphics.GL.Raw.Profile.Core43
import System.IO.Unsafe
import Text.ParserCombinators.ReadP

parse :: String -> Maybe Version
parse s = listToMaybe [v | (v,"") <- readP_to_S parseVersion s ]
  
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

-- | Returns a version or release number.
version :: Version
version = Version [] [] `fromMaybe` parse versionString

shadingLanguageVersionString :: String
shadingLanguageVersionString = unsafePerformIO $ getString GL_SHADING_LANGUAGE_VERSION
{-# NOINLINE shadingLanguageVersionString #-}

-- | Return the primary shading language version
shadingLanguageVersion :: Version
shadingLanguageVersion = Version [] [] `fromMaybe` parse shadingLanguageVersionString

-- | Returns a set of shading language versions supported by this implementation.
shadingLanguageVersions :: Set Version
shadingLanguageVersions = unsafePerformIO $ do
  n <- alloca $ \p -> do
    poke p 0 -- unsupported until 4.2, so scribble a 0 in first
    glGetIntegerv GL_NUM_SHADING_LANGUAGE_VERSIONS p
    peek p
  versions <- forM [0..fromIntegral n-1] $ \i -> do
    cs <- glGetStringi GL_SHADING_LANGUAGE_VERSION i
    parse <$> peekCString (castPtr cs)
  return $ if n == 0
    then Set.singleton shadingLanguageVersion
    else Set.fromList $ catMaybes versions

{-# NOINLINE shadingLanguageVersions #-}
