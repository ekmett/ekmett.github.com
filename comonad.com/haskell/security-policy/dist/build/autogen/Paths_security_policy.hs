module Paths_security_policy (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version

version = Version {versionBranch = [0,0,1], versionTags = []}

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/security-policy-0.0.1/ghc-6.6.1"
datadir    = "/usr/local/share/security-policy-0.0.1"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "/" ++ name)
