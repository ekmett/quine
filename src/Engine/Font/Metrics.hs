{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
module Engine.Font.Metrics 
  ( Glyph(..)
  , HasGlyph(..)
  , Metrics(..)
  , HasMetrics(..)
  ) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad (mzero)
import Data.Aeson
import Data.Data
import Data.Map as Map
import Data.Text
import GHC.Generics

data Glyph = Glyph
  { _glyphWidth, _glyphHeight, _glyphBearingX, _glyphBearingY, _glyphAdvance :: !Int
  , _glyphPos :: Maybe (Int, Int)
  } deriving (Show, Read, Typeable, Generic, Data)

makeClassy ''Glyph

data Metrics = Metrics
  { _metricsFontFamily, _metricsFontStyle :: !Text
  , _metricsBuffer,     _metricsSize      :: {-# UNPACK #-} !Int
  , _metricsChars                         :: !(Map Text Glyph)
  } deriving (Show, Read, Typeable, Generic, Data)

makeClassy ''Metrics

instance FromJSON Metrics where
  parseJSON = withObject "Metrics" $ \v -> Metrics 
    <$> v .: "family"
    <*> v .: "style"
    <*> v .: "buffer"
    <*> v .: "size"
    <*> v .: "chars"

instance ToJSON Metrics where
  toJSON (Metrics f st b s cs) = object
    [ "family" .= f
    , "style"  .= st
    , "buffer" .= b
    , "size"   .= s
    , "chars"  .= cs
    ]

instance FromJSON Glyph where
  parseJSON a = parseJSON a >>= \ case
    w:h:bx:by:ba:ps -> Glyph w h bx by ba <$> case ps of
      [px,py] -> return $ Just (px,py)
      []      -> return Nothing
      _       -> mzero
    _ -> mzero

instance ToJSON Glyph where
  toJSON (Glyph w h bx by ba ps) = toJSON $ w:h:bx:by:ba: case ps of 
     Just (px,py) -> [px,py]
     Nothing -> []

--
-- drawAt :: Metrics -> Int -> Int -> String -> IO ()
-- drawAt 
