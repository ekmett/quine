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
  , pattern Patches
  -- $usage
  , pattern LineStripWithAdjacent
  , pattern LinesWithAdjacent
  , pattern TrianglesWithAdjacent
  , pattern TriangleStripWithAdjacent
  -- * Draw Calls
  -- ** Indirect Arrays
  , DrawArraysIndirectCommand(..)
  , drawArrayIndirect
  -- ** Indirect Elements
  , DrawElementsIndirectCommand(..)
  , drawElementsIndirect
  ) where


import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Data.Data
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types

type DrawMode = GLenum

-- * Draw Calls

-- ** Indirect Arrays

data DrawArraysIndirectCommand = DrawArraysIndirectCommand { aCount, aPrimCount, aFirstIdx, __reserved :: Word32 }
  deriving (Show,Read,Eq,Typeable,Data,Generic)

instance Storable DrawArraysIndirectCommand where
  sizeOf _ = 4 * sizeOf (undefined::Word32)
  alignment _ = alignment (undefined::Word32)
  peek ptr =
    let wPtr = castPtr ptr
    in DrawArraysIndirectCommand <$> peekElemOff wPtr 0 <*> peekElemOff wPtr 1 <*> peekElemOff wPtr 2 <*> peekElemOff wPtr 3
  poke ptr DrawArraysIndirectCommand{..} =
    let wPtr = castPtr ptr
    in pokeElemOff wPtr 0 aCount >> pokeElemOff wPtr 1 aPrimCount >> pokeElemOff wPtr 2 aFirstIdx >> pokeElemOff wPtr 3 __reserved

-- | Draws multiple primitives
--
-- Arguments:
--
-- [DrawMode] one of the 3.3+ supported modes
--
-- [Offset]   an offset pointer into the currently bound 'DrawIndirectBuffer' (in machine units)  
--
drawArrayIndirect :: MonadIO m => DrawMode -> Ptr DrawArraysIndirectCommand -> m ()
drawArrayIndirect mode = liftIO . glDrawArraysIndirect mode . castPtr

-- ** Indirect Elements

data DrawElementsIndirectCommand = DrawElementsIndirectCommand { eCount, ePrimCount, eFirstIdx, eBaseVertex, eBaseInstance :: Word32 }
  deriving (Show,Read,Eq,Typeable,Data,Generic)

instance Storable DrawElementsIndirectCommand where
  sizeOf _ = 4 * sizeOf (undefined::Word32)
  alignment _ = alignment (undefined::Word32)
  peek ptr =
    let wPtr = castPtr ptr
    in DrawElementsIndirectCommand <$> peekElemOff wPtr 0 <*> peekElemOff wPtr 1 <*> peekElemOff wPtr 2 <*> peekElemOff wPtr 3 <*> peekElemOff wPtr 4
  poke ptr DrawElementsIndirectCommand{..} =
    let wPtr = castPtr ptr
    in zipWithM_ (pokeElemOff wPtr) [0..] [eCount, ePrimCount, eFirstIdx, eBaseVertex, eBaseInstance]

-- | Draws multiple primitives from the a set of elements
--
-- Arguments:
--
-- [DrawMode] one of the 3.3+ supported modes
--
-- [Type]     The type of the indices of the currently bound 'ElementArrayBuffer' ('GL_UNSIGNED_BYTE', 'GL_UNSIGNED_SHORT', 'GL_UNSIGNED_INT')
--
-- [Offset]   an offset pointer into the currently bound 'DrawIndirectBuffer' (in machine units)  
--
drawElementsIndirect :: MonadIO m => DrawMode -> GLenum -> Ptr DrawElementsIndirectCommand -> m ()
drawElementsIndirect mode ty = liftIO . glDrawElementsIndirect mode ty . castPtr

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

-- | Patches for tesselation
pattern Patches = GL_PATCHES

