module CacheFile where

import System.FilePath

indexFile :: String
indexFile = "00-index.tar.gz"

hackportDir :: String
hackportDir = ".hackport"

hackageDir :: String
hackageDir = "hackage.haskell.org"

cacheFile :: FilePath -> FilePath
cacheFile tree = tree </> hackportDir </> hackageDir </> indexFile
