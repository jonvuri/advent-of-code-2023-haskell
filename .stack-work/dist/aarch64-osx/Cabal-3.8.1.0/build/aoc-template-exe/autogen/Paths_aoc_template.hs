{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_aoc_template (
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
version = Version [2023,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/jonvuri/Dev/aoc2023-haskell/.stack-work/install/aarch64-osx/46b50883d86961a27d5500336cb0b1cdc105deb667c5b1d2115fa8f2c37482c7/9.4.6/bin"
libdir     = "/Users/jonvuri/Dev/aoc2023-haskell/.stack-work/install/aarch64-osx/46b50883d86961a27d5500336cb0b1cdc105deb667c5b1d2115fa8f2c37482c7/9.4.6/lib/aarch64-osx-ghc-9.4.6/aoc-template-2023.0-keDeLjnMk53J8f5G4q06a-aoc-template-exe"
dynlibdir  = "/Users/jonvuri/Dev/aoc2023-haskell/.stack-work/install/aarch64-osx/46b50883d86961a27d5500336cb0b1cdc105deb667c5b1d2115fa8f2c37482c7/9.4.6/lib/aarch64-osx-ghc-9.4.6"
datadir    = "/Users/jonvuri/Dev/aoc2023-haskell/.stack-work/install/aarch64-osx/46b50883d86961a27d5500336cb0b1cdc105deb667c5b1d2115fa8f2c37482c7/9.4.6/share/aarch64-osx-ghc-9.4.6/aoc-template-2023.0"
libexecdir = "/Users/jonvuri/Dev/aoc2023-haskell/.stack-work/install/aarch64-osx/46b50883d86961a27d5500336cb0b1cdc105deb667c5b1d2115fa8f2c37482c7/9.4.6/libexec/aarch64-osx-ghc-9.4.6/aoc-template-2023.0"
sysconfdir = "/Users/jonvuri/Dev/aoc2023-haskell/.stack-work/install/aarch64-osx/46b50883d86961a27d5500336cb0b1cdc105deb667c5b1d2115fa8f2c37482c7/9.4.6/etc"

getBinDir     = catchIO (getEnv "aoc_template_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "aoc_template_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "aoc_template_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "aoc_template_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc_template_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc_template_sysconfdir") (\_ -> return sysconfdir)




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
