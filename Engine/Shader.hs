module Engine.Shader where

import Language.Preprocessor.Cpphs
import Paths_engine

opts :: CpphsOptions
opts = defaultCpphsOptions 
  { boolopts = defaultBoolOptions 
    { macros = False
    , locations = True
    , hashline = True
    , pragma = True
    , stripEol = True
    , stripC89 = True
    , lang = False
    , ansi = False
    , layout = True
    , literate = False
    , warnings = True
    }
  }

cpphs :: [(String,String)] -> FilePath -> IO String
cpphs ds fp = do
  fp' <- getDataFileName fp
  content <- readFile fp'
  dataDir <- getDataDir
  runCpphs opts { defines = ds, includes = [dataDir, "data"] } fp' content
