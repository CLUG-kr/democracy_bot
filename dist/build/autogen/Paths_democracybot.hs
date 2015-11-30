module Paths_democracybot (
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

bindir     = "/home/codeonwort/haskell/projects/democracybot/.cabal-sandbox/bin"
libdir     = "/home/codeonwort/haskell/projects/democracybot/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/democracybot-0.1.0.0"
datadir    = "/home/codeonwort/haskell/projects/democracybot/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/democracybot-0.1.0.0"
libexecdir = "/home/codeonwort/haskell/projects/democracybot/.cabal-sandbox/libexec"
sysconfdir = "/home/codeonwort/haskell/projects/democracybot/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "democracybot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "democracybot_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "democracybot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "democracybot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "democracybot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
