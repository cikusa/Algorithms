module Paths_genetic_algorithms (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/linghui/.cabal/bin"
libdir     = "/Users/linghui/.cabal/lib/x86_64-osx-ghc-7.10.2/genetic-algorithms-0.1.0.0-FsKaua73ZnRE8aJ9WLSwpm"
datadir    = "/Users/linghui/.cabal/share/x86_64-osx-ghc-7.10.2/genetic-algorithms-0.1.0.0"
libexecdir = "/Users/linghui/.cabal/libexec"
sysconfdir = "/Users/linghui/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "genetic_algorithms_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "genetic_algorithms_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "genetic_algorithms_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "genetic_algorithms_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "genetic_algorithms_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
