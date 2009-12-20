module Paths_cabal_install (
    version,
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,8,0], versionTags = []}
