{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_TP2 (
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

bindir     = "/workspaces/ALP/TP2/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/46ef71b7e3aa11000f13096bd6e16db34ceefa63666d86a12536886f0617c548/8.8.3/bin"
libdir     = "/workspaces/ALP/TP2/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/46ef71b7e3aa11000f13096bd6e16db34ceefa63666d86a12536886f0617c548/8.8.3/lib/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0-A6A8gGDsYeBEWsy64eFdOq-TP2-exe"
dynlibdir  = "/workspaces/ALP/TP2/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/46ef71b7e3aa11000f13096bd6e16db34ceefa63666d86a12536886f0617c548/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/workspaces/ALP/TP2/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/46ef71b7e3aa11000f13096bd6e16db34ceefa63666d86a12536886f0617c548/8.8.3/share/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0"
libexecdir = "/workspaces/ALP/TP2/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/46ef71b7e3aa11000f13096bd6e16db34ceefa63666d86a12536886f0617c548/8.8.3/libexec/x86_64-linux-ghc-8.8.3/TP2-0.1.0.0"
sysconfdir = "/workspaces/ALP/TP2/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/46ef71b7e3aa11000f13096bd6e16db34ceefa63666d86a12536886f0617c548/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TP2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TP2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TP2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TP2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
