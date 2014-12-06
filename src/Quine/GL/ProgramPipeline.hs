{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Separable 'Programs's can be combined to a 'ProgramPipeline'
-- Requires: 4.1+ or GL_ARB_separate_shader_objects
--------------------------------------------------------------------
module Quine.GL.ProgramPipeline
  ( ProgramPipeline
  , PipelineStage
  , boundProgramPipeline
  , useProgramStages
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Default
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import Quine.StateVar
import Quine.GL.Object
import Quine.GL.Program

-- | A Pipeline object captures shader stages
newtype ProgramPipeline = ProgramPipeline GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

type PipelineStage = GLbitfield

instance Object ProgramPipeline where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsProgramPipeline (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteProgramPipelines (fromIntegral n) p
    where n = length xs

instance Gen ProgramPipeline where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenProgramPipelines (fromIntegral n) p
    map ProgramPipeline <$> peekArray n p

instance Default ProgramPipeline where
  def = ProgramPipeline 0

boundProgramPipeline :: StateVar ProgramPipeline
boundProgramPipeline = StateVar g s where
  g = fmap (ProgramPipeline . fromIntegral) $ alloca $ liftM2 (>>) (glGetIntegerv GL_PROGRAM_PIPELINE_BINDING) peek
  s = glBindProgramPipeline . coerce

useProgramStages :: MonadIO m => ProgramPipeline -> PipelineStage -> Program -> m ()
useProgramStages pipe stage prog = glUseProgramStages (coerce pipe) stage (coerce prog)
