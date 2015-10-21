module Paths_empty (
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

bindir     = "/home/xikusa/dev/algorithms/empty/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/bin"
libdir     = "/home/xikusa/dev/algorithms/empty/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/lib/x86_64-linux-ghc-7.10.2/empty-0.1.0.0-JRPIeSaAhYjHF1ZrrCELjk"
datadir    = "/home/xikusa/dev/algorithms/empty/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/share/x86_64-linux-ghc-7.10.2/empty-0.1.0.0"
libexecdir = "/home/xikusa/dev/algorithms/empty/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/libexec"
sysconfdir = "/home/xikusa/dev/algorithms/empty/.stack-work/install/x86_64-linux/lts-3.9/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "empty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "empty_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "empty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "empty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "empty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
