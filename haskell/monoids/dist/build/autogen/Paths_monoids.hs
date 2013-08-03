module Paths_monoids (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,29], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/ekmett/.cabal/bin"
libdir     = "/home/ekmett/.cabal/lib/monoids-0.1.29/ghc-6.10.1"
datadir    = "/home/ekmett/.cabal/share/monoids-0.1.29"
libexecdir = "/home/ekmett/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "monoids_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "monoids_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "monoids_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "monoids_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
