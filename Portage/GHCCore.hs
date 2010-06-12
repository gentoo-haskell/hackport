
-- Guess GHC version from packages depended upon.
module Portage.GHCCore where

import Distribution.Package
import Distribution.Version
import Distribution.Simple.PackageIndex
import Distribution.InstalledPackageInfo

import Distribution.Text

import Data.Maybe ( fromJust )
import Data.Monoid

import Text.PrettyPrint.HughesPJ

packageIsCore :: PackageIndex -> PackageName -> Bool
packageIsCore index pn = not . null $ lookupPackageName index pn

all_index :: PackageIndex
all_index = mconcat [ ghc6121_index, ghc6104_index, ghc682_index ]

-- | Check if a dependency is satisfiable given a 'PackageIndex'
-- representing the core packages in a GHC version.
-- Packages that are not core will always be accepted, packages that are
-- core must be satisfied by the 'PackageIndex'.
dependencySatisfiable :: PackageIndex -> Dependency -> Bool
dependencySatisfiable pi dep@(Dependency pn rang)
  | pn == PackageName "Win32" = False
  | not (packageIsCore pi pn) = True
  | otherwise = not . null $ lookupDependency pi dep

latest_index :: PackageIndex
latest_index = ghc6121_index

mkIndex :: [PackageIdentifier] -> PackageIndex
mkIndex pids = fromList
  [ emptyInstalledPackageInfo
      { installedPackageId = InstalledPackageId $ display name ++ "-" ++  display version
      , sourcePackageId = pi
      , exposed = True
      }
  | pi@(PackageIdentifier name version) <- pids ]


-- | Core packages in GHC 6.12.1 as a 'PackageIndex'.
ghc6121_index :: PackageIndex
ghc6121_index = mkIndex ghc6121

ghc6104_index :: PackageIndex
ghc6104_index = mkIndex ghc6104

ghc682_index :: PackageIndex
ghc682_index = mkIndex ghc682

-- | Core packages in GHC 6.12.1
ghc6121 :: [PackageIdentifier]
ghc6121 =
  [ p "array" [0,3,0,0]
  , p "base" [3,0,3,2]
  , p "base" [4,2,0,0]
  , p "bytestring" [0,9,1,5]
  , p "Cabal" [1,8,0,2]
  , p "containers" [0,3,0,0]
  , p "directory" [1,0,1,0]
  , p "extensible-exceptions" [0,1,1,1]
  , p "filepath" [1,1,0,3]
  , p "haskell98" [1,0,1,1]
  , p "hpc" [0,5,0,4]
  , p "integer-smp" [0,2,0,0]
  , p "integer-simple" [0,1,0,0]
  , p "old-locale" [1,0,0,2]
  , p "old-time" [1,0,0,3]
  , p "pretty" [1,0,1,1]
  , p "process" [1,0,1,2]
  , p "random" [1,0,0,2]
  , p "syb" [0,1,0,2]
  , p "template-haskell" [2,4,0,0]
  , p "unix" [2,4,0,0]
  , p "utf8-string" [0,3,4]
  ]

ghc6104 :: [PackageIdentifier]
ghc6104 =
  [ p "array" [0,2,0,0]
  , p "base" [3,0,3,1]
  , p "base" [4,1,0,0]
  , p "bytestring" [0,9,1,4]
  , p "Cabal" [1,6,0,3]
  , p "containers" [0,2,0,1 ]
  , p "directory" [1,0,0,3]
  , p "extensible-exceptions" [0,1,1,0]
  , p "filepath" [1,1,0,2]
  , p "haskell98" [1,0,1,0]
  , p "hpc" [0,5,0,3]
  , p "old-locale" [1,0,0,1]
  , p "old-time" [1,0,0,2]
  , p "packedstring" [0,1,0,1]
  , p "pretty" [1,0,1,0]
  , p "process" [1,0,1,1]
  , p "random" [1,0,0,1]
  , p "syb" [0,1,0,1]
  , p "template-haskell" [2,3,0,1]
  , p "time" [1,1,4]
  , p "unix" [2,3,2,0]
  ]

ghc682_pkgs :: [PackageIdentifier]
ghc682_pkgs =
  [ p "array" [0,1,0,0]
  , p "base" [3,0,1,0]
  , p "bytestring" [0,9,0,1]
--  , p "Cabal" [1,2,3,0] package is upgradeable
  , p "containers" [0,1,0,1]
  , p "dictionary" [1,0,0,0]
  , p "filepath" [1,1,0,0]
  , p "haskell98" [1,0,1,0]
  , p "hpc" [0,5,0,0]
  , p "old-locale" [1,0,0,0]
  , p "old-time" [1,0,0,0]
  , p "packedstring" [0,1,0,0]
  , p "pretty" [1,0,0,0]
  , p "process" [1,0,0,0]
  , p "random" [1,0,0,0]
--  , p "readline" [1,0,1,0]
  , p "template-haskell" [2,2,0,0]
  , p "unix" [2,3,0,0]
  ]

p :: String -> [Int] -> PackageIdentifier
p pn vs = PackageIdentifier (PackageName pn) (Version vs [])
