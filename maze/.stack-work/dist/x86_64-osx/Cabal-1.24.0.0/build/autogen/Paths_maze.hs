{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_maze (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/cht/Documents/personal/maze/.stack-work/install/x86_64-osx/nightly-2016-07-28/8.0.1/bin"
libdir     = "/Users/cht/Documents/personal/maze/.stack-work/install/x86_64-osx/nightly-2016-07-28/8.0.1/lib/x86_64-osx-ghc-8.0.1/maze-0.1.0.0"
datadir    = "/Users/cht/Documents/personal/maze/.stack-work/install/x86_64-osx/nightly-2016-07-28/8.0.1/share/x86_64-osx-ghc-8.0.1/maze-0.1.0.0"
libexecdir = "/Users/cht/Documents/personal/maze/.stack-work/install/x86_64-osx/nightly-2016-07-28/8.0.1/libexec"
sysconfdir = "/Users/cht/Documents/personal/maze/.stack-work/install/x86_64-osx/nightly-2016-07-28/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "maze_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "maze_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "maze_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "maze_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "maze_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
