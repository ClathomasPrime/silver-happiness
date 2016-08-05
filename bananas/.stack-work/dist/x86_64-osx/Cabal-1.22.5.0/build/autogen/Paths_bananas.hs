module Paths_bananas (
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

bindir     = "/Users/cht/Documents/personal/bananas/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/bin"
libdir     = "/Users/cht/Documents/personal/bananas/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/lib/x86_64-osx-ghc-7.10.3/bananas-0.1.0.0-Cp2xwv5VopMJUBlkEAKghU"
datadir    = "/Users/cht/Documents/personal/bananas/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/share/x86_64-osx-ghc-7.10.3/bananas-0.1.0.0"
libexecdir = "/Users/cht/Documents/personal/bananas/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/libexec"
sysconfdir = "/Users/cht/Documents/personal/bananas/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bananas_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bananas_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bananas_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bananas_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bananas_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
