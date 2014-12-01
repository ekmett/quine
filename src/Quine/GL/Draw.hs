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
  ( PrimitiveMode
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

type PrimitiveMode = GLenum
type IndexType = GLenum

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
-- [PrimitiveMode] one of the 3.3+ supported modes
--
-- [Offset]   an offset pointer into the currently bound 'DrawIndirectBuffer' (in machine units)  
--
drawArrayIndirect :: MonadIO m => PrimitiveMode -> Ptr DrawArraysIndirectCommand -> m ()
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
-- [PrimitiveMode]
--    one of the 3.3+ supported modes
--
-- [IndexType]
--    The type of the indices of the currently bound 'ElementArrayBuffer' ('GL_UNSIGNED_BYTE', 'GL_UNSIGNED_SHORT', 'GL_UNSIGNED_INT')
--
-- [Offset]     
--    an offset pointer into the currently bound 'DrawIndirectBuffer' (in machine units)  
--
drawElementsIndirect :: MonadIO m => PrimitiveMode -> IndexType -> Ptr DrawElementsIndirectCommand -> m ()
drawElementsIndirect mode ty = liftIO . glDrawElementsIndirect mode ty . castPtr

