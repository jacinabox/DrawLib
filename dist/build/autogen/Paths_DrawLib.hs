{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_DrawLib (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\james\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\james\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\DrawLib-0.1.0.0-6yvtR3XSwKOFhUT1Y37U7N"
dynlibdir  = "C:\\Users\\james\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1"
datadir    = "C:\\Users\\james\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\DrawLib-0.1.0.0"
libexecdir = "C:\\Users\\james\\AppData\\Roaming\\cabal\\DrawLib-0.1.0.0-6yvtR3XSwKOFhUT1Y37U7N\\x86_64-windows-ghc-8.2.1\\DrawLib-0.1.0.0"
sysconfdir = "C:\\Users\\james\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DrawLib_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DrawLib_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "DrawLib_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "DrawLib_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DrawLib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DrawLib_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
