{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Data
import Data.Default
import Data.Int
import Data.Set
import Data.Word
import GHC.Generics
import Graphics.UI.SDL
import Graphics.UI.SDL.Enum.Pattern

data Input = Input
  { _keyCodes        :: Set Int32
  , _scanCodes       :: Set Word32
  , _mouseX          :: Int32
  , _mouseY          :: Int32
  , _mouseButtons    :: Set Word8
  , _mouseButtonMask :: Word32
  , _mouseRelX       :: Int32 -- relative since last reset
  , _mouseRelY       :: Int32 -- relative since last reset
  , _mouseWheelX     :: Int32 -- relative since last reset
  , _mouseWheelY     :: Int32 -- relative since last reset
  } deriving (Show, Typeable, Generic)

makeClassy ''Input

instance Default Input where
  def = Input def def def def def def def def def def
  
handleInputEvent :: (MonadState s m, HasInput s) => Event -> m ()
handleInputEvent (KeyboardEvent EventTypeKeyDown _ _ _ _ (Keysym sc kc _)) = do
  keyCodes.contains kc .= True 
  scanCodes.contains sc .= True
handleInputEvent (KeyboardEvent EventTypeKeyUp _ _ _ _ (Keysym sc kc _)) = do
  keyCodes.contains kc  .= False
  scanCodes.contains sc .= False
handleInputEvent (MouseMotionEvent EventTypeMouseMotion _ _ mouse mask x y relx rely) | mouse /= TouchMouseID = do
  mouseX .= x
  mouseY .= y
  mouseButtonMask .= mask
  mouseRelX += relx
  mouseRelY += rely
handleInputEvent (MouseButtonEvent EventTypeMouseButtonDown _ _ mouse button _ _ x y) | mouse /= TouchMouseID = do
  mouseX .= x
  mouseY .= y
  mouseButtons.contains button .= True
  when (button == ButtonLeft)   $ mouseButtonMask .|.= ButtonLMask
  when (button == ButtonMiddle) $ mouseButtonMask .|.= ButtonMMask
  when (button == ButtonRight)  $ mouseButtonMask .|.= ButtonRMask
  when (button == ButtonX1)     $ mouseButtonMask .|.= ButtonX1Mask
  when (button == ButtonX2)     $ mouseButtonMask .|.= ButtonX2Mask
handleInputEvent (MouseButtonEvent EventTypeMouseButtonDown _ _ mouse button _ _ x y) | mouse /= TouchMouseID = do
  mouseX .= x
  mouseY .= y
  mouseButtons.contains button .= False
  when (button == ButtonLeft)   $ mouseButtonMask .&.= complement ButtonLMask
  when (button == ButtonMiddle) $ mouseButtonMask .&.= complement ButtonMMask
  when (button == ButtonRight)  $ mouseButtonMask .&.= complement ButtonRMask
  when (button == ButtonX1)     $ mouseButtonMask .&.= complement ButtonX1Mask
  when (button == ButtonX2)     $ mouseButtonMask .&.= complement ButtonX2Mask
handleInputEvent (MouseWheelEvent EventTypeMouseWheel _ _ mouse x y) | mouse /= TouchMouseID = do
  mouseWheelX += x
  mouseWheelY += y
handleInputEvent _ = return ()

