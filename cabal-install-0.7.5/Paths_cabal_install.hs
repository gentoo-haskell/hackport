module Paths_cabal_install (
    version,
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,7,5], versionTags = []}
