{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_TP2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/mnt/c/Users/luisa/OneDrive/Desktop/ALP/.stack-work/install/x86_64-linux-tinfo6/5cb63dff7111491d1920ad5826a03153cc0fd64290fa425c408871e72ba5549c/9.4.7/bin"
libdir     = "/mnt/c/Users/luisa/OneDrive/Desktop/ALP/.stack-work/install/x86_64-linux-tinfo6/5cb63dff7111491d1920ad5826a03153cc0fd64290fa425c408871e72ba5549c/9.4.7/lib/x86_64-linux-ghc-9.4.7/TP2-0.1.0.0-C8J8x8rOowT2AU7k91o9iI"
dynlibdir  = "/mnt/c/Users/luisa/OneDrive/Desktop/ALP/.stack-work/install/x86_64-linux-tinfo6/5cb63dff7111491d1920ad5826a03153cc0fd64290fa425c408871e72ba5549c/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/mnt/c/Users/luisa/OneDrive/Desktop/ALP/.stack-work/install/x86_64-linux-tinfo6/5cb63dff7111491d1920ad5826a03153cc0fd64290fa425c408871e72ba5549c/9.4.7/share/x86_64-linux-ghc-9.4.7/TP2-0.1.0.0"
libexecdir = "/mnt/c/Users/luisa/OneDrive/Desktop/ALP/.stack-work/install/x86_64-linux-tinfo6/5cb63dff7111491d1920ad5826a03153cc0fd64290fa425c408871e72ba5549c/9.4.7/libexec/x86_64-linux-ghc-9.4.7/TP2-0.1.0.0"
sysconfdir = "/mnt/c/Users/luisa/OneDrive/Desktop/ALP/.stack-work/install/x86_64-linux-tinfo6/5cb63dff7111491d1920ad5826a03153cc0fd64290fa425c408871e72ba5549c/9.4.7/etc"

getBinDir     = catchIO (getEnv "TP2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "TP2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "TP2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "TP2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP2_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
