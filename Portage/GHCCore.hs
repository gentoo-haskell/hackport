
-- Guess GHC version from packages depended upon.
module Portage.GHCCore
        ( minimumGHCVersionToBuildPackage
        , cabalFromGHC
        , finalizePackageDescription
        , platform
        , dependencySatisfiable
        ) where

import qualified Distribution.Compiler as DC
import Distribution.Package
import Distribution.Version
import Distribution.Simple.PackageIndex
import Distribution.InstalledPackageInfo as IPI

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
ghcs :: [(DC.CompilerInfo, InstalledPackageIndex)]
ghcs = modern_ghcs
    where modern_ghcs  = [ghc741, ghc742, ghc761, ghc762, ghc782, ghc7101, ghc7102, ghc801]

cabalFromGHC :: [Int] -> Maybe Version
cabalFromGHC ver = lookup ver table
  where
  table = [ ([7,4,2],  Version [1,14,0] [])
          , ([7,6,1],  Version [1,16,0] [])
          , ([7,6,2],  Version [1,16,0] [])
          , ([7,8,2],  Version [1,18,1,3] [])
          , ([7,10,1], Version [1,22,2,0] [])
          , ([7,10,2], Version [1,22,4,0] [])
          , ([8,0,1],  Version [1,24,0,0] [])
          ]

platform :: Platform
platform = Platform X86_64 Linux

packageIsCore :: InstalledPackageIndex -> PackageName -> Bool
packageIsCore index pn = not . null $ lookupPackageName index pn

packageIsCoreInAnyGHC :: PackageName -> Bool
packageIsCoreInAnyGHC pn = any (flip packageIsCore pn) (map snd ghcs)

-- | Check if a dependency is satisfiable given a 'PackageIndex'
-- representing the core packages in a GHC version.
-- Packages that are not core will always be accepted, packages that are
-- core in any ghc must be satisfied by the 'PackageIndex'.
dependencySatisfiable :: InstalledPackageIndex -> Dependency -> Bool
dependencySatisfiable pindex dep@(Dependency pn _rang)
  | pn == PackageName "Win32" = False -- only exists on windows, not in linux
  | not . null $ lookupDependency pindex dep = True -- the package index satisfies the dep
  | packageIsCoreInAnyGHC pn = False -- some other ghcs support the dependency
  | otherwise = True -- the dep is not related with core packages, accept the dep

packageBuildableWithGHCVersion
  :: GenericPackageDescription
  -> FlagAssignment
  -> (DC.CompilerInfo, InstalledPackageIndex)
  -> Either [Dependency] (PackageDescription, FlagAssignment)
packageBuildableWithGHCVersion pkg user_specified_fas (compiler_info, pkgIndex) = trace_failure $
  finalizePackageDescription user_specified_fas (dependencySatisfiable pkgIndex) platform compiler_info [] pkg
    where trace_failure v = case v of
              (Left deps) -> trace (unwords ["rejecting dep:" , show_compiler compiler_info
                                            , "as", show_deps deps
                                            , "were not found."
                                            ]
                                   ) v
              _           -> trace (unwords ["accepting dep:" , show_compiler compiler_info
                                            ]
                                   ) v
          show_deps = show . map display
          show_compiler (DC.CompilerInfo { DC.compilerInfoId = CompilerId GHC v }) = "ghc-" ++ showVersion v
          show_compiler c = show c

-- | Given a 'GenericPackageDescription' it returns the miminum GHC version
-- to build a package, and a list of core packages to that GHC version.
minimumGHCVersionToBuildPackage :: GenericPackageDescription -> FlagAssignment -> Maybe (DC.CompilerInfo, [PackageName], PackageDescription, FlagAssignment, InstalledPackageIndex)
minimumGHCVersionToBuildPackage gpd user_specified_fas =
  listToMaybe [ (cinfo, packageNamesFromPackageIndex pix, pkg_desc, picked_flags, pix)
              | g@(cinfo, pix) <- ghcs
              , Right (pkg_desc, picked_flags) <- return (packageBuildableWithGHCVersion gpd user_specified_fas g)]

mkIndex :: [PackageIdentifier] -> InstalledPackageIndex
mkIndex pids = fromList
  [ emptyInstalledPackageInfo
      { sourcePackageId = pindex
      , exposed = True
      }
  | pindex@(PackageIdentifier _name _version) <- pids ]

packageNamesFromPackageIndex :: InstalledPackageIndex -> [PackageName]
packageNamesFromPackageIndex pix = nub $ map fst $ allPackagesByName pix

ghc :: [Int] -> DC.CompilerInfo
ghc nrs = DC.unknownCompilerInfo c_id DC.NoAbiTag
    where c_id = CompilerId GHC (Version nrs [])

ghc801 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc801 = (ghc [8,0,1], mkIndex ghc801_pkgs)

ghc7102 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc7102 = (ghc [7,10,2], mkIndex ghc7102_pkgs)

ghc7101 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc7101 = (ghc [7,10,1], mkIndex ghc7101_pkgs)

ghc782 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc782 = (ghc [7,8,2], mkIndex ghc782_pkgs)

ghc762 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc762 = (ghc [7,6,2], mkIndex ghc762_pkgs)

ghc761 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc761 = (ghc [7,6,1], mkIndex ghc761_pkgs)

ghc742 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc742 = (ghc [7,4,2], mkIndex ghc742_pkgs)

ghc741 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc741 = (ghc [7,4,1], mkIndex ghc741_pkgs)

-- | Non-upgradeable core packages
-- Source: http://haskell.org/haskellwiki/Libraries_released_with_GHC
--         and our binary tarballs (package.conf.d.initial subdir)

ghc801_pkgs :: [PackageIdentifier]
ghc801_pkgs =
  [ p "array" [0,5,1,1]
  , p "base" [4,9,0,0]
  , p "binary" [0,8,3,0] -- used by libghc
  , p "bytestring" [0,10,8,0]
--  , p "Cabal" [1,24,0,0]  package is upgradeable
  , p "containers" [0,5,7,1]
  , p "deepseq" [1,4,2,0] -- used by time
  , p "directory" [1,2,6,2]
  , p "filepath" [1,4,1,0]
  , p "ghc-boot" [8,0,1]
  , p "ghc-prim" [0,5,0,0]
  , p "ghci" [8,0,1]
  , p "hoopl" [3,10,2,1] -- used by libghc
  , p "hpc" [0,6,0,3] -- used by libghc
  , p "integer-gmp" [1,0,0,1]
  , p "pretty" [1,1,3,3]
  , p "process" [1,4,2,0]
  , p "template-haskell" [2,11,0,0] -- used by libghc
  -- , p "terminfo" [0,4,0,2]
  , p "time" [1,6,0,1] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,2,0] -- used by libghc
  , p "unix" [2,7,2,0]
--  , p "xhtml" [3000,2,1]
  ]

ghc7102_pkgs :: [PackageIdentifier]
ghc7102_pkgs =
  [ p "array" [0,5,1,0]
  , p "base" [4,8,1,0]
  , p "binary" [0,7,3,0] -- used by libghc
  , p "bytestring" [0,10,6,0]
--  , p "Cabal" [1,18,1,3]  package is upgradeable
  , p "containers" [0,5,6,2]
  , p "deepseq" [1,4,1,1] -- used by time
  , p "directory" [1,2,2,0]
  , p "filepath" [1,4,0,0]
  , p "ghc-prim" [0,4,0,0]
  -- , p "haskell2010" [1,1,2,0] -- stopped shipping in 7.10, deprecated
  -- , p "haskell98" [2,0,0,3] -- stopped shipping in 7.10, deprecated
  , p "hoopl" [3,10,1,0] -- used by libghc
  , p "hpc" [0,6,0,2] -- used by libghc
  , p "integer-gmp" [1,0,0,0]
  -- , p "old-locale" [1,0,0,6] -- stopped shipping in 7.10, deprecated
  -- , p "old-time" [1,1,0,2] -- stopped shipping in 7.10, deprecated
  , p "pretty" [1,1,2,0]
  , p "process" [1,2,3,0]
  , p "template-haskell" [2,10,0,0] -- used by libghc
  , p "time" [1,5,0,1] -- used by haskell98, unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,4,2,0] -- used by libghc
  , p "unix" [2,7,1,0]
  ]

ghc7101_pkgs :: [PackageIdentifier]
ghc7101_pkgs =
  [ p "array" [0,5,1,0]
  , p "base" [4,8,0,0]
  , p "binary" [0,7,3,0] -- used by libghc
  , p "bytestring" [0,10,6,0]
--  , p "Cabal" [1,18,1,3]  package is upgradeable
  , p "containers" [0,5,6,2]
  , p "deepseq" [1,4,1,1] -- used by time
  , p "directory" [1,2,2,0]
  , p "filepath" [1,4,0,0]
  , p "ghc-prim" [0,4,0,0]
  -- , p "haskell2010" [1,1,2,0] -- stopped shipping in 7.10, deprecated
  -- , p "haskell98" [2,0,0,3] -- stopped shipping in 7.10, deprecated
  , p "hoopl" [3,10,0,2] -- used by libghc
  , p "hpc" [0,6,0,2] -- used by libghc
  , p "integer-gmp" [1,0,0,0]
  -- , p "old-locale" [1,0,0,6] -- stopped shipping in 7.10, deprecated
  -- , p "old-time" [1,1,0,2] -- stopped shipping in 7.10, deprecated
  , p "pretty" [1,1,2,0]
  , p "process" [1,2,3,0]
  , p "template-haskell" [2,10,0,0] -- used by libghc
  , p "time" [1,5,0,1] -- used by haskell98, unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,4,2,0] -- used by libghc
  , p "unix" [2,7,1,0]
  ]

ghc782_pkgs :: [PackageIdentifier]
ghc782_pkgs =
  [ p "array" [0,5,0,0]
  , p "base" [4,7,0,0]
  , p "binary" [0,7,1,0] -- used by libghc
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
  , p "transformers" [0,3,0,0] -- used by libghc
  , p "unix" [2,7,0,1]
  ]

ghc762_pkgs :: [PackageIdentifier]
ghc762_pkgs =
  [ p "array" [0,4,0,1]
  , p "base" [4,6,0,1]
  , p "binary" [0,5,1,1] -- used by libghc
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
  , p "binary" [0,5,1,1] -- used by libghc
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
  , p "binary" [0,5,1,0] -- used by libghc
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
  , p "binary" [0,5,1,0] -- used by libghc
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
