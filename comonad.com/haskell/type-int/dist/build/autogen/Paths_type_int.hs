module Paths_type_int (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version

version = Version {versionBranch = [0,4], versionTags = []}

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/type-int-0.4/ghc-6.6.1"
datadir    = "/usr/local/share/type-int-0.4"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "/" ++ name)
