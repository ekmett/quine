{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
module Engine.Options where

import Control.Applicative
import Control.Lens
import Data.Data
import Data.Default
import Engine.Monitor.Options
import GHC.Generics
import Options.Applicative
import Paths_engine
import Prelude hiding (init)

data Options = Options
  { _optionsMonitorOptions    :: MonitorOptions
  , _optionsFullScreen        :: !Bool
  , _optionsFullScreenDesktop :: !Bool
  , _optionsHighDPI           :: !Bool
  , _optionsWindowWidth       :: !Int
  , _optionsWindowHeight      :: !Int
  , _optionsDataDir :: !FilePath
  } deriving (Generic,Data,Typeable)

makeClassy ''Options

instance HasMonitorOptions Options where
  monitorOptions = optionsMonitorOptions

-- we need to set up the data directory first
parseOptions :: IO (Parser Options)
parseOptions = do
  dd <- getDataDir
  return $ Options 
       <$> parseMonitorOptions
       <*> switch (long "full-screen" <> short 'f' <> help "open full-screen on launch")
       <*> switch (long "full-screen-to-desktop" <> short 'n' <> help "keep desktop resolution when going full screen")
       <*> switch (long "retina" <> short 'r' <> help "exploit a retina display if it available")
       <*> option auto (long "width" <> short 'x' <> help "window width in pixels" <> metavar "WIDTH" <> value 1024)
       <*> option auto (long "height" <> short 'y' <> help "window height in pixels" <> metavar "HEIGHT" <> value 768)
       <*> option auto (long "data" <> short 'd' <> help "location of the data directory" <> metavar "DIR" <> action "directory" <> value dd)

instance Default Options where
  def = Options def False False False 1024 768 "data"
