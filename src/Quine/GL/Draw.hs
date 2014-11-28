{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE PatternSynonyms    #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Tell OpenGL to draw something
--------------------------------------------------------------------
module Quine.GL.Draw
  ( DrawMode
  -- * Drawing Primitives
  , pattern LineLoop
  , pattern Lines
  , pattern Points
  , pattern TriangleFan
  , pattern Triangles
  , pattern TriangleStrip
  -- $usage
  , pattern LineStripWithAdjacent
  , pattern LinesWithAdjacent
  , pattern TrianglesWithAdjacent
  , pattern TriangleStripWithAdjacent
  -- * Indirect Drawing
  , DrawArraysIndirectCommand(..)
  , drawArrayIndirect
  ) where


import Control.Applicative
import Control.Monad.IO.Class
import Data.Data
import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types

type DrawMode = GLenum

data DrawArraysIndirectCommand = DrawArraysIndirectCommand { count, primCount, first, reserved :: Word32 }
  deriving (Show,Read,Eq,Typeable,Data,Generic)

instance Storable DrawArraysIndirectCommand where
  sizeOf _ = 4 * sizeOf (undefined::Word32)
  alignment _ = alignment (undefined::Word32)
  peek ptr =
    let wPtr = castPtr ptr
    in DrawArraysIndirectCommand <$> peekElemOff wPtr 0 <*> peekElemOff wPtr 1 <*> peekElemOff wPtr 2 <*> peekElemOff wPtr 3
  poke ptr DrawArraysIndirectCommand{..} =
    let wPtr = castPtr ptr
    in pokeElemOff wPtr 0 count >> pokeElemOff wPtr 1 primCount >> pokeElemOff wPtr 2 first >> pokeElemOff wPtr 3 reserved

-- | Draws multipe primitives from the 
drawArrayIndirect :: MonadIO m => DrawMode -> DrawArraysIndirectCommand -> m ()
drawArrayIndirect mode cmd = liftIO . with cmd $ glDrawArraysIndirect mode . castPtr

-- * Drawing Primitves

-- $usage
--
-- Terminology:
--
-- [WithAdjacent] Special 'DrawMode' for geometry shaders to give them access to adjacent primitives

-- | Closed Line from first to last to first
pattern LineLoop = GL_LINE_LOOP

-- | Separate lines: (0,1), (2,3), ...
pattern Lines = GL_LINES

-- | Lines with gs access to adjacent lines
pattern LinesWithAdjacent = GL_LINES_ADJACENCY

-- | Adjacent connected lines: (0,1), (1,2), ...
pattern LineStripWithAdjacent = GL_LINE_STRIP

-- | Points
pattern Points = GL_POINTS

-- | Triangle fan around first vertex: (0,1,2), (0,3,4), ...
pattern TriangleFan = GL_TRIANGLE_FAN

-- | Separate triangles: (0,1,2), (3,4,5), ...
pattern Triangles = GL_TRIANGLES

-- | Triangles with gs access to the adjacent triangles
pattern TrianglesWithAdjacent = GL_TRIANGLES_ADJACENCY

-- | Adjacent connected triangles: (0,1,2), (2,1,3), (2, 3, 4), ...
pattern TriangleStrip = GL_TRIANGLE_STRIP

-- | Adjacent connected triangles with gs access to adjacent
pattern TriangleStripWithAdjacent = GL_TRIANGLE_STRIP_ADJACENCY
