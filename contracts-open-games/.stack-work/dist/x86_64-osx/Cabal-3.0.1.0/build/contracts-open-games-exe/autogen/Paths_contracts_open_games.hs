{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_contracts_open_games (
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

bindir     = "/Users/lancendavis/Documents/Metagov/contracts-algebra/contracts-open-games/.stack-work/install/x86_64-osx/10d044ec1e806f11d75da06b38e876f6a8a0269551794b6ad5b072200db58108/8.8.4/bin"
libdir     = "/Users/lancendavis/Documents/Metagov/contracts-algebra/contracts-open-games/.stack-work/install/x86_64-osx/10d044ec1e806f11d75da06b38e876f6a8a0269551794b6ad5b072200db58108/8.8.4/lib/x86_64-osx-ghc-8.8.4/contracts-open-games-0.1.0.0-IrZQHXsMe9t3K90Kjn6W8N-contracts-open-games-exe"
dynlibdir  = "/Users/lancendavis/Documents/Metagov/contracts-algebra/contracts-open-games/.stack-work/install/x86_64-osx/10d044ec1e806f11d75da06b38e876f6a8a0269551794b6ad5b072200db58108/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/lancendavis/Documents/Metagov/contracts-algebra/contracts-open-games/.stack-work/install/x86_64-osx/10d044ec1e806f11d75da06b38e876f6a8a0269551794b6ad5b072200db58108/8.8.4/share/x86_64-osx-ghc-8.8.4/contracts-open-games-0.1.0.0"
libexecdir = "/Users/lancendavis/Documents/Metagov/contracts-algebra/contracts-open-games/.stack-work/install/x86_64-osx/10d044ec1e806f11d75da06b38e876f6a8a0269551794b6ad5b072200db58108/8.8.4/libexec/x86_64-osx-ghc-8.8.4/contracts-open-games-0.1.0.0"
sysconfdir = "/Users/lancendavis/Documents/Metagov/contracts-algebra/contracts-open-games/.stack-work/install/x86_64-osx/10d044ec1e806f11d75da06b38e876f6a8a0269551794b6ad5b072200db58108/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "contracts_open_games_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "contracts_open_games_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "contracts_open_games_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "contracts_open_games_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "contracts_open_games_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "contracts_open_games_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
