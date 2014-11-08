{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
-- | 
-- Do we need to hook window entry to get a better state summary of the mouse buttons to avoid the common 'sticky mouse' problems in games?
-- 
-- Should losing focus kill the mouse button states?
module Quine.Input
  ( Input(..)
  , HasInput(..)
  , handleInputEvent
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.State.Class
import Data.Bits
import Data.Bits.Lens
import Data.Default
import Data.Int
import Data.Set
import Data.Word
import Graphics.UI.SDL
import Graphics.UI.SDL.Enum.Pattern
import Linear

data Input = Input
  { _keyCodes        :: !(Set Int32)
  , _scanCodes       :: !(Set Word32)
  , _mouseButtons    :: !(Set Word8)
  , _mousePos        :: !(V2 Int32)
  , _mouseButtonMask :: !Word32
  , _mouseRel        :: !(V2 Int32) -- relative since last reset
  , _mouseWheel      :: !(V2 Int32) -- relative since last reset
  } deriving Show

makeClassy ''Input

instance Default Input where
  def = Input def def def 0 0 0 0
  
handleInputEvent :: (MonadState s m, HasInput s) => Event -> m ()
handleInputEvent (KeyboardEvent EventTypeKeyDown _ _ _ _ (Keysym sc kc _)) = do
  keyCodes.contains kc .= True 
  scanCodes.contains sc .= True
handleInputEvent (KeyboardEvent EventTypeKeyUp _ _ _ _ (Keysym sc kc _)) = do
  keyCodes.contains kc  .= False
  scanCodes.contains sc .= False
handleInputEvent (MouseMotionEvent EventTypeMouseMotion _ _ mouse mask x y relx rely) | mouse /= TouchMouseID = do
  mousePos .= V2 x y
  mouseButtonMask .= mask
  mouseRel += V2 relx rely
handleInputEvent (MouseButtonEvent EventTypeMouseButtonDown _ _ mouse button _ _ x y) | mouse /= TouchMouseID = do
  mousePos .= V2 x y
  mouseButtons.contains button .= True
  when (button == ButtonLeft)   $ mouseButtonMask .|.= ButtonLMask
  when (button == ButtonMiddle) $ mouseButtonMask .|.= ButtonMMask
  when (button == ButtonRight)  $ mouseButtonMask .|.= ButtonRMask
  when (button == ButtonX1)     $ mouseButtonMask .|.= ButtonX1Mask
  when (button == ButtonX2)     $ mouseButtonMask .|.= ButtonX2Mask
handleInputEvent (MouseButtonEvent EventTypeMouseButtonDown _ _ mouse button _ _ x y) | mouse /= TouchMouseID = do
  mousePos .= V2 x y
  mouseButtons.contains button .= False
  when (button == ButtonLeft)   $ mouseButtonMask .&.= complement ButtonLMask
  when (button == ButtonMiddle) $ mouseButtonMask .&.= complement ButtonMMask
  when (button == ButtonRight)  $ mouseButtonMask .&.= complement ButtonRMask
  when (button == ButtonX1)     $ mouseButtonMask .&.= complement ButtonX1Mask
  when (button == ButtonX2)     $ mouseButtonMask .&.= complement ButtonX2Mask
handleInputEvent (MouseWheelEvent EventTypeMouseWheel _ _ mouse x y) | mouse /= TouchMouseID =
  mouseWheel += V2 x y
handleInputEvent _ = return ()

