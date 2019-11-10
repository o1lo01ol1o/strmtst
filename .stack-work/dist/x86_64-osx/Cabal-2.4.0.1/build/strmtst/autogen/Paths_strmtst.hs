{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_strmtst (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/timpierson/arity/strmtst/.stack-work/install/x86_64-osx/e1b1c61671dbb449089c8230e4f0e31a028f257e144b8fbe7920dac5a30fd35f/8.6.5/bin"
libdir     = "/Users/timpierson/arity/strmtst/.stack-work/install/x86_64-osx/e1b1c61671dbb449089c8230e4f0e31a028f257e144b8fbe7920dac5a30fd35f/8.6.5/lib/x86_64-osx-ghc-8.6.5/strmtst-0.0.0-ERSP2OOPjwxMVDy01dANc-strmtst"
dynlibdir  = "/Users/timpierson/arity/strmtst/.stack-work/install/x86_64-osx/e1b1c61671dbb449089c8230e4f0e31a028f257e144b8fbe7920dac5a30fd35f/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/timpierson/arity/strmtst/.stack-work/install/x86_64-osx/e1b1c61671dbb449089c8230e4f0e31a028f257e144b8fbe7920dac5a30fd35f/8.6.5/share/x86_64-osx-ghc-8.6.5/strmtst-0.0.0"
libexecdir = "/Users/timpierson/arity/strmtst/.stack-work/install/x86_64-osx/e1b1c61671dbb449089c8230e4f0e31a028f257e144b8fbe7920dac5a30fd35f/8.6.5/libexec/x86_64-osx-ghc-8.6.5/strmtst-0.0.0"
sysconfdir = "/Users/timpierson/arity/strmtst/.stack-work/install/x86_64-osx/e1b1c61671dbb449089c8230e4f0e31a028f257e144b8fbe7920dac5a30fd35f/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "strmtst_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "strmtst_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "strmtst_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "strmtst_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "strmtst_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "strmtst_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
