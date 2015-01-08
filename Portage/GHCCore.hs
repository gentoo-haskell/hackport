
-- Guess GHC version from packages depended upon.
module Portage.GHCCore
        ( minimumGHCVersionToBuildPackage
        , cabalFromGHC
        , finalizePackageDescription
        , platform
        , dependencySatisfiable
        ) where

import Distribution.Package
import Distribution.Version
import Distribution.Simple.PackageIndex
import Distribution.InstalledPackageInfo

import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Compiler (CompilerId(..), CompilerFlavor(GHC))
import Distribution.System

import Distribution.Text

import Data.Maybe
import Data.List ( nub )
import Data.Version

import Debug.Trace

-- ghcs tried in specified order.
-- It means that first ghc in this list is a minmum default.
ghcs :: [(CompilerId, PackageIndex)]
ghcs = modern_ghcs
    where modern_ghcs  = [ghc741, ghc742, ghc761, ghc762, ghc782]

cabalFromGHC :: [Int] -> Maybe Version
cabalFromGHC ver = lookup ver table
  where
  table = [ ([7,4,2],  Version [1,14,0] [])
          , ([7,6,1],  Version [1,16,0] [])
          , ([7,6,2],  Version [1,16,0] [])
          , ([7,8,2],  Version [1,18,1,3] [])
          ]

platform :: Platform
platform = Platform X86_64 Linux

packageIsCore :: PackageIndex -> PackageName -> Bool
packageIsCore index pn = not . null $ lookupPackageName index pn

packageIsCoreInAnyGHC :: PackageName -> Bool
packageIsCoreInAnyGHC pn = any (flip packageIsCore pn) (map snd ghcs)

-- | Check if a dependency is satisfiable given a 'PackageIndex'
-- representing the core packages in a GHC version.
-- Packages that are not core will always be accepted, packages that are
-- core in any ghc must be satisfied by the 'PackageIndex'.
dependencySatisfiable :: PackageIndex -> Dependency -> Bool
dependencySatisfiable pindex dep@(Dependency pn _rang)
  | pn == PackageName "Win32" = False -- only exists on windows, not in linux
  | not . null $ lookupDependency pindex dep = True -- the package index satisfies the dep
  | packageIsCoreInAnyGHC pn = False -- some other ghcs support the dependency
  | otherwise = True -- the dep is not related with core packages, accept the dep

packageBuildableWithGHCVersion
  :: GenericPackageDescription
  -> FlagAssignment
  -> (CompilerId, PackageIndex)
  -> Either [Dependency] (PackageDescription, FlagAssignment)
packageBuildableWithGHCVersion pkg user_specified_fas (compiler, pkgIndex) = trace_failure $
  finalizePackageDescription user_specified_fas (dependencySatisfiable pkgIndex) platform compiler [] pkg
    where trace_failure v = case v of
              (Left deps) -> trace (unwords ["rejecting dep:" , show_compiler compiler
                                            , "as", show_deps deps
                                            , "were not found."
                                            ]
                                   ) v
              _           -> trace (unwords ["accepting dep:" , show_compiler compiler
                                            ]
                                   ) v
          show_deps = show . map display
          show_compiler (CompilerId GHC v) = "ghc-" ++ showVersion v
          show_compiler c = show c

-- | Given a 'GenericPackageDescription' it returns the miminum GHC version
-- to build a package, and a list of core packages to that GHC version.
minimumGHCVersionToBuildPackage :: GenericPackageDescription -> FlagAssignment -> Maybe (CompilerId, [PackageName], PackageDescription, FlagAssignment, PackageIndex)
minimumGHCVersionToBuildPackage gpd user_specified_fas =
  listToMaybe [ (cid, packageNamesFromPackageIndex pix, pkg_desc, picked_flags, pix)
              | g@(cid, pix) <- ghcs
              , Right (pkg_desc, picked_flags) <- return (packageBuildableWithGHCVersion gpd user_specified_fas g)]

mkIndex :: [PackageIdentifier] -> PackageIndex
mkIndex pids = fromList
  [ emptyInstalledPackageInfo
      { installedPackageId = InstalledPackageId $ display name ++ "-" ++  display version
      , sourcePackageId = pindex
      , exposed = True
      }
  | pindex@(PackageIdentifier name version) <- pids ]

packageNamesFromPackageIndex :: PackageIndex -> [PackageName]
packageNamesFromPackageIndex pix = nub $ map fst $ allPackagesByName pix

ghc :: [Int] -> CompilerId
ghc nrs = CompilerId GHC (Version nrs [])

ghc782 :: (CompilerId, PackageIndex)
ghc782 = (ghc [7,8,2], mkIndex ghc782_pkgs)

ghc762 :: (CompilerId, PackageIndex)
ghc762 = (ghc [7,6,2], mkIndex ghc762_pkgs)

ghc761 :: (CompilerId, PackageIndex)
ghc761 = (ghc [7,6,1], mkIndex ghc761_pkgs)

ghc742 :: (CompilerId, PackageIndex)
ghc742 = (ghc [7,4,2], mkIndex ghc742_pkgs)

ghc741 :: (CompilerId, PackageIndex)
ghc741 = (ghc [7,4,1], mkIndex ghc741_pkgs)

-- | Non-upgradeable core packages
-- Source: http://haskell.org/haskellwiki/Libraries_released_with_GHC

ghc782_pkgs :: [PackageIdentifier]
ghc782_pkgs =
  [ p "array" [0,5,0,0]
  , p "base" [4,7,0,0]
--  , p "binary" [0,7,1,0]  package is upgradeable
  , p "bytestring" [0,10,4,0]
--  , p "Cabal" [1,18,1,3]  package is upgradeable
  , p "containers" [0,5,5,1]
  , p "deepseq" [1,3,0,2] -- used by time, haskell98
  , p "directory" [1,2,1,0]
  , p "filepath" [1,3,0,2]
  , p "ghc-prim" [0,3,1,0]
  , p "haskell2010" [1,1,2,0]
  , p "haskell98" [2,0,0,3]
  , p "hoopl" [3,10,0,1] -- used by libghc
  , p "hpc" [0,6,0,1] -- used by libghc
  , p "integer-gmp" [0,5,1,0]
  -- , p "old-locale" [1,0,0,6] -- stopped shipping in 7.10, deprecated
  -- , p "old-time" [1,1,0,2] -- stopped shipping in 7.10, deprecated
  , p "pretty" [1,1,1,1]
  , p "process" [1,2,0,0]
  , p "template-haskell" [2,9,0,0] -- used by libghc
  , p "time" [1,4,2] -- used by haskell98, unix, directory, hpc, ghc. unsafe to upgrade
--  , p "transformers" [0,3,0,0] -- used by libghc
  , p "unix" [2,7,0,1]
  ]

ghc762_pkgs :: [PackageIdentifier]
ghc762_pkgs =
  [ p "array" [0,4,0,1]
  , p "base" [4,6,0,1]
--  , p "binary" [0,5,1,1]  package is upgradeable
  , p "bytestring" [0,10,0,2]
--  , p "Cabal" [1,16,0]  package is upgradeable
  , p "containers" [0,5,0,0]
  , p "deepseq" [1,3,0,1] -- used by time, haskell98
  , p "directory" [1,2,0,1]
  , p "filepath" [1,3,0,1]
  , p "ghc-prim" [0,3,0,0]
  , p "haskell2010" [1,1,1,0]
  , p "haskell98" [2,0,0,2]
  , p "hoopl" [3,9,0,0] -- used by libghc
  , p "hpc" [0,6,0,0] -- used by libghc
  , p "integer-gmp" [0,5,0,0]
  -- , p "old-locale" [1,0,0,5] -- stopped shipping in 7.10, deprecated
  -- , p "old-time" [1,1,0,1] -- stopped shipping in 7.10, deprecated
  , p "pretty" [1,1,1,0]
  , p "process" [1,1,0,2]
  , p "template-haskell" [2,8,0,0] -- used by libghc
  , p "time" [1,4,0,1] -- used by haskell98, unix, directory, hpc, ghc. unsafe to upgrade
  , p "unix" [2,6,0,1]
  ]

ghc761_pkgs :: [PackageIdentifier]
ghc761_pkgs =
  [ p "array" [0,4,0,1]
  , p "base" [4,6,0,0]
--  , p "binary" [0,5,1,1]  package is upgradeable
  , p "bytestring" [0,10,0,0]
--  , p "Cabal" [1,16,0]  package is upgradeable
  , p "containers" [0,5,0,0]
  , p "deepseq" [1,3,0,1] -- used by time, haskell98
  , p "directory" [1,2,0,0]
  , p "filepath" [1,3,0,1]
  , p "ghc-prim" [0,3,0,0]
  , p "haskell2010" [1,1,1,0]
  , p "haskell98" [2,0,0,2]
  , p "hoopl" [3,9,0,0] -- used by libghc
  , p "hpc" [0,6,0,0] -- used by libghc
  , p "integer-gmp" [0,5,0,0]
  -- , p "old-locale" [1,0,0,5] -- stopped shipping in 7.10, deprecated
  -- , p "old-time" [1,1,0,1] -- stopped shipping in 7.10, deprecated
  , p "pretty" [1,1,1,0]
  , p "process" [1,1,0,2]
  , p "template-haskell" [2,8,0,0] -- used by libghc
  , p "time" [1,4,0,1] -- used by haskell98, unix, directory, hpc, ghc. unsafe to upgrade
  , p "unix" [2,6,0,0]
  ]

ghc742_pkgs :: [PackageIdentifier]
ghc742_pkgs =
  [ p "array" [0,4,0,0]
  , p "base" [4,5,1,0]
--  , p "binary" [0,5,1,0]  package is upgradeable
  , p "bytestring" [0,9,2,1]
--  , p "Cabal" [1,14,0]  package is upgradeable
  , p "containers" [0,4,2,1]
  , p "deepseq" [1,3,0,0] -- used by time, haskell98
  , p "directory" [1,1,0,2]
-- , p "extensible-exceptions" [0,1,1,4] -- package is upgradeable, stopped shipping in 7.6
  , p "filepath" [1,3,0,0]
  , p "ghc-prim" [0,2,0,0]
  , p "haskell2010" [1,1,0,1]
  , p "haskell98" [2,0,0,1]
  , p "hoopl" [3,8,7,3] -- used by libghc
  , p "hpc" [0,5,1,1] -- used by libghc
  , p "integer-gmp" [0,4,0,0]
  -- , p "old-locale" [1,0,0,4] -- stopped shipping in 7.10, deprecated
  -- , p "old-time" [1,1,0,0] -- stopped shipping in 7.10, deprecated
  , p "pretty" [1,1,1,0]
  , p "process" [1,1,0,1]
  , p "template-haskell" [2,7,0,0] -- used by libghc
  , p "time" [1,4]
  , p "unix" [2,5,1,1]
  ]

ghc741_pkgs :: [PackageIdentifier]
ghc741_pkgs =
  [ p "array" [0,4,0,0]
  , p "base" [4,5,0,0]
--  , p "binary" [0,5,1,0]  package is upgradeable
  , p "bytestring" [0,9,2,1]
--  , p "Cabal" [1,14,0]  package is upgradeable
  , p "containers" [0,4,2,1]
  , p "deepseq" [1,3,0,0] -- used by time, haskell98
  , p "directory" [1,1,0,2]
-- , p "extensible-exceptions" [0,1,1,4] -- package is upgradeable, stopped shipping in 7.6
  , p "filepath" [1,3,0,0]
  , p "ghc-prim" [0,2,0,0]
  , p "haskell2010" [1,1,0,1]
  , p "haskell98" [2,0,0,1]
  , p "hoopl" [3,8,7,3] -- used by libghc
  , p "hpc" [0,5,1,1] -- used by libghc
  , p "integer-gmp" [0,4,0,0]
  -- , p "old-locale" [1,0,0,4] -- stopped shipping in 7.10, deprecated
  -- , p "old-time" [1,1,0,0] -- stopped shipping in 7.10, deprecated
  , p "pretty" [1,1,1,0]
  , p "process" [1,1,0,1]
  , p "template-haskell" [2,7,0,0] -- used by libghc
  , p "time" [1,4]
  , p "unix" [2,5,1,0]
  ]

p :: String -> [Int] -> PackageIdentifier
p pn vs = PackageIdentifier (PackageName pn) (Version vs [])
