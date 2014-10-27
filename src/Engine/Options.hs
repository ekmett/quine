{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
module Engine.Options where

-- import Control.Applicative
import Control.Lens
import Data.Data
import Data.Default
import GHC.Generics
import Paths_engine
import Prelude hiding (init)

data Options = Options
  { _optionsFullScreen :: !Bool
  } deriving (Generic,Data,Typeable)

makeClassy ''Options

parseOptions :: IO Options
parseOptions = do
  _dd <- getDataDir
  return $ Options True 

instance Default Options where
  def = Options True
