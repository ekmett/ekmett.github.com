module Paths_buffer (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/ekmett/.cabal/bin"
libdir     = "/home/ekmett/.cabal/lib/buffer-0.0.1/ghc-6.10.4"
datadir    = "/home/ekmett/.cabal/share/buffer-0.0.1"
libexecdir = "/home/ekmett/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "buffer_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "buffer_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "buffer_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "buffer_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
