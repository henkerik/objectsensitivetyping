module Paths_cco (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,4], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/henkerik/.cabal/bin"
libdir     = "/home/henkerik/.cabal/lib/cco-0.0.4/ghc-7.4.1"
datadir    = "/home/henkerik/.cabal/share/cco-0.0.4"
libexecdir = "/home/henkerik/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "cco_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cco_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cco_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cco_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
