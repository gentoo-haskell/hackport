module Paths_cabal_install (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,6,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/kolmodin/.cabal/bin"
libdir     = "/home/kolmodin/.cabal/lib/cabal-install-0.6.0/ghc-6.8.3"
datadir    = "/home/kolmodin/.cabal/share/cabal-install-0.6.0"
libexecdir = "/home/kolmodin/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "cabal_install_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "cabal_install_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "cabal_install_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "cabal_install_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
