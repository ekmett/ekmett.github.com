module Paths_monad_sec (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version

version = Version {versionBranch = [0,0,1], versionTags = []}

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/monad-sec-0.0.1/ghc-6.6.1"
datadir    = "/usr/local/share/monad-sec-0.0.1"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "/" ++ name)
