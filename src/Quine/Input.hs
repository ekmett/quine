{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Do we need to hook window entry to get a better state summary of the mouse buttons to avoid the common 'sticky mouse' problems in games?
-- 
-- Should losing focus kill the mouse button states?
--------------------------------------------------------------------
module Quine.Input
  ( Input(..)
  , HasInput(..)
  , handleInputEvent
  ) where

import Control.Lens
import Control.Monad.State.Class
import Data.Bits.Lens
import Data.Default
import Data.Int
import Data.Set
import Data.Word
import SDL.Raw
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
handleInputEvent (KeyboardEvent SDL_KEYDOWN _ _ _ _ (Keysym sc kc _)) = do
  keyCodes.contains kc .= True 
  scanCodes.contains sc .= True
handleInputEvent (KeyboardEvent SDL_KEYUP _ _ _ _ (Keysym sc kc _)) = do
  keyCodes.contains kc  .= False
  scanCodes.contains sc .= False
handleInputEvent (MouseMotionEvent SDL_MOUSEMOTION _ _ mouse mask x y relx rely) | mouse /= SDL_TOUCH_MOUSEID = do
  mousePos .= V2 x y
  mouseButtonMask .= mask
  mouseRel += V2 relx rely
handleInputEvent (MouseButtonEvent SDL_MOUSEBUTTONDOWN _ _ mouse button _ _ x y) | mouse /= SDL_TOUCH_MOUSEID = do
  mousePos .= V2 x y
  mouseButtons.contains button .= True
  mouseButtonMask.bitAt (fromIntegral button) .= True
handleInputEvent (MouseButtonEvent SDL_MOUSEBUTTONUP _ _ mouse button _ _ x y) | mouse /= SDL_TOUCH_MOUSEID = do
  mousePos .= V2 x y
  mouseButtons.contains button .= False
  mouseButtonMask.bitAt (fromIntegral button) .= False
handleInputEvent (MouseWheelEvent SDL_MOUSEWHEEL _ _ mouse x y) | mouse /= SDL_TOUCH_MOUSEID =
  mouseWheel += V2 x y
handleInputEvent _ = return ()

