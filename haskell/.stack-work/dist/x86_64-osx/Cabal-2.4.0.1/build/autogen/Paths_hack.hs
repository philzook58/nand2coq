{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hack (
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

bindir     = "/Users/philip/Documents/coq/nand2coq/haskell/.stack-work/install/x86_64-osx/478f13c6d3b966ba1b28e0344df1035a0b5bb0b730526079b8fe39128ca28a10/8.6.5/bin"
libdir     = "/Users/philip/Documents/coq/nand2coq/haskell/.stack-work/install/x86_64-osx/478f13c6d3b966ba1b28e0344df1035a0b5bb0b730526079b8fe39128ca28a10/8.6.5/lib/x86_64-osx-ghc-8.6.5/hack-0.1.0.0-GPQvDH8LQqNG9baVVTzefK"
dynlibdir  = "/Users/philip/Documents/coq/nand2coq/haskell/.stack-work/install/x86_64-osx/478f13c6d3b966ba1b28e0344df1035a0b5bb0b730526079b8fe39128ca28a10/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/philip/Documents/coq/nand2coq/haskell/.stack-work/install/x86_64-osx/478f13c6d3b966ba1b28e0344df1035a0b5bb0b730526079b8fe39128ca28a10/8.6.5/share/x86_64-osx-ghc-8.6.5/hack-0.1.0.0"
libexecdir = "/Users/philip/Documents/coq/nand2coq/haskell/.stack-work/install/x86_64-osx/478f13c6d3b966ba1b28e0344df1035a0b5bb0b730526079b8fe39128ca28a10/8.6.5/libexec/x86_64-osx-ghc-8.6.5/hack-0.1.0.0"
sysconfdir = "/Users/philip/Documents/coq/nand2coq/haskell/.stack-work/install/x86_64-osx/478f13c6d3b966ba1b28e0344df1035a0b5bb0b730526079b8fe39128ca28a10/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hack_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hack_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hack_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hack_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hack_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hack_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
