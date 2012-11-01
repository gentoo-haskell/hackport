
-- Guess GHC version from packages depended upon.
module Portage.GHCCore
        ( minimumGHCVersionToBuildPackage
        , cabalFromGHC
        , defaultGHC
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

defaultGHC :: (CompilerId, [PackageName])
defaultGHC = let (g,pix) = ghc6123 in (g, packageNamesFromPackageIndex pix)

ghcs :: [(CompilerId, PackageIndex)]
ghcs = [ghc6104, ghc6121, ghc6122, ghc6123, ghc701, ghc742, ghc761]

cabalFromGHC :: [Int] -> Maybe Version
cabalFromGHC ver = lookup ver table
  where
  table = [([6,6,0],  Version [1,1,6] [])
          ,([6,6,1],  Version [1,1,6,2] [])
          ,([6,8,1],  Version [1,2,2,0] [])
          ,([6,8,2],  Version [1,2,3,0] [])
          ,([6,8,3],  Version [1,2,4,0] [])
          ,([6,10,1], Version [1,6,0,1] [])
          ,([6,10,2], Version [1,6,0,3] [])
          ,([6,10,3], Version [1,6,0,3] [])
          ,([6,10,4], Version [1,6,0,3] [])
          ,([6,12,1], Version [1,8,0,2] [])
          ,([6,12,2], Version [1,8,0,4] [])
          ,([6,12,3], Version [1,8,0,6] [])
          ,([7,0,1],  Version [1,10,0,0] [])
          ,([7,4,2],  Version [1,14,0] [])
          ,([7,6,1],  Version [1,16,0] [])
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
dependencySatisfiable pi dep@(Dependency pn rang)
  | pn == PackageName "Win32" = False -- only exists on windows, not in linux
  | not . null $ lookupDependency pi dep = True -- the package index satisfies the dep
  | packageIsCoreInAnyGHC pn = False -- some other ghcs support the dependency
  | otherwise = True -- the dep is not related with core packages, accept the dep

packageBuildableWithGHCVersion
  :: GenericPackageDescription
  -> (CompilerId, PackageIndex)
  -> Either [Dependency] (PackageDescription, FlagAssignment)
packageBuildableWithGHCVersion pkg (compiler, pkgIndex) = trace_failure $
  finalizePackageDescription [] (dependencySatisfiable pkgIndex) platform compiler [] pkg
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
minimumGHCVersionToBuildPackage :: GenericPackageDescription -> Maybe (CompilerId, [PackageName])
minimumGHCVersionToBuildPackage gpd =
  listToMaybe [ (cid, packageNamesFromPackageIndex pix)
              | g@(cid, pix) <- ghcs
              , Right _ <- return (packageBuildableWithGHCVersion gpd g)]

mkIndex :: [PackageIdentifier] -> PackageIndex
mkIndex pids = fromList
  [ emptyInstalledPackageInfo
      { installedPackageId = InstalledPackageId $ display name ++ "-" ++  display version
      , sourcePackageId = pi
      , exposed = True
      }
  | pi@(PackageIdentifier name version) <- pids ]

packageNamesFromPackageIndex :: PackageIndex -> [PackageName]
packageNamesFromPackageIndex pix = nub $ map fst $ allPackagesByName pix

ghc :: [Int] -> CompilerId
ghc nrs = CompilerId GHC (Version nrs [])

ghc761 :: (CompilerId, PackageIndex)
ghc761 = (ghc [7,6,1], mkIndex ghc761_pkgs)

ghc742 :: (CompilerId, PackageIndex)
ghc742 = (ghc [7,4,2], mkIndex ghc742_pkgs)

ghc701 :: (CompilerId, PackageIndex)
ghc701 = (ghc [7,0,1], mkIndex ghc701_pkgs)

ghc6123 :: (CompilerId, PackageIndex)
ghc6123 = (ghc [6,12,3], mkIndex ghc6123_pkgs)

ghc6122 :: (CompilerId, PackageIndex)
ghc6122 = (ghc [6,12,2], mkIndex ghc6122_pkgs)

ghc6121 :: (CompilerId, PackageIndex)
ghc6121 = (ghc [6,12,1], mkIndex ghc6121_pkgs)

ghc6104 :: (CompilerId, PackageIndex)
ghc6104 = (ghc [6,10,4], mkIndex ghc6104_pkgs)

-- | Non-upgradeable core packages
-- Source: http://haskell.org/haskellwiki/Libraries_released_with_GHC

ghc761_pkgs :: [PackageIdentifier]
ghc761_pkgs =
  [ p "array" [0,4,0,1]
  , p "base" [4,6,0,0]
--  , p "binary" [0,5,1,1]  package is upgradeable
  , p "bytestring" [0,10,0,8]
--  , p "Cabal" [1,16,0]  package is upgradeable
  , p "containers" [0,5,0,0]
--  , p "deepseq" [1,3,0,1] -- package is upgradeable
  , p "directory" [1,2,0,0]
  , p "filepath" [1,3,0,1]
  , p "ghc-prim" [0,3,0,0]
  , p "haskell2010" [1,1,1,0]
  , p "haskell98" [2,0,0,2]
  , p "hoopl" [3,9,0,0] -- used by libghc
  , p "hpc" [0,6,0,0] -- used by libghc
  , p "integer-gmp" [0,5,0,0]
  , p "old-locale" [1,0,0,5]
  , p "old-time" [1,1,0,1]
  , p "pretty" [1,1,1,0]
  , p "process" [1,1,0,2]
  , p "template-haskell" [2,8,0,0] -- used by libghc
-- , p "time" [1,4,0,1] -- upgradeable, but used by haskell98
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
  , p "directory" [1,1,0,2]
-- , p "extensible-exceptions" [0,1,1,4] -- package is upgradeable, stopped shipping in 7.6
  , p "filepath" [1,3,0,0]
  , p "ghc-prim" [0,2,0,0]
  , p "haskell2010" [1,1,0,1]
  , p "haskell98" [2,0,0,1]
  , p "hoopl" [3,8,7,3] -- used by libghc
  , p "hpc" [0,5,1,1] -- used by libghc
  , p "integer-gmp" [0,4,0,0]
  , p "old-locale" [1,0,0,4]
  , p "old-time" [1,1,0,0]
  , p "pretty" [1,1,1,0]
  , p "process" [1,1,0,1]
  , p "template-haskell" [2,7,0,0] -- used by libghc
-- , p "time" [1,4] -- upgradeable, but used by haskell98
  , p "unix" [2,5,1,1]
  ]

ghc701_pkgs :: [PackageIdentifier]
ghc701_pkgs =
  [ p "array" [0,3,0,2]
  , p "base" [4,3,0,0]
  , p "bytestring" [0,9,1,8]
--  , p "Cabal" [1,10,0,0]  package is upgradeable
  , p "containers" [0,4,0,0]
  , p "directory" [1,1,0,0]
-- , p "extensible-exceptions" [0,1,1,2] -- package is upgradeable, stopped shipping in 7.6
  , p "filepath" [1,2,0,0]
  , p "haskell2010" [1,0,0,0]
  , p "haskell98" [1,1,0,0]
  , p "hpc" [0,5,0,6]
  , p "integer-gmp" [0,2,0,2]
  , p "integer-simple" [0,1,0,0]
  , p "old-locale" [1,0,0,2]
  , p "old-time" [1,0,0,6]
  , p "pretty" [1,0,1,2]
  , p "process" [1,0,1,4]
--   , p "random" [1,0,0,3] -- will not be shipped starting from ghc-7.2
  , p "template-haskell" [2,5,0,0]
--  , p "time" [1,2,0,3] package is upgradeable
  , p "unix" [2,4,1,0]
--  , p "utf8-string" [0,3,4] package is upgradeable
  ]

ghc6123_pkgs :: [PackageIdentifier]
ghc6123_pkgs =
  [ p "array" [0,3,0,1]
  , p "base" [3,0,3,2]
  , p "base" [4,2,0,2]
  , p "bytestring" [0,9,1,7]
--  , p "Cabal" [1,8,0,6]  package is upgradeable
  , p "containers" [0,3,0,0]
  , p "directory" [1,0,1,1]
-- , p "extensible-exceptions" [0,1,1,1] -- package is upgradeable, stopped shipping in 7.6
  , p "filepath" [1,1,0,4]
  , p "haskell98" [1,0,1,1]
  , p "hpc" [0,5,0,5]
  , p "integer-gmp" [0,2,0,1]
  , p "integer-simple" [0,1,0,0]
  , p "old-locale" [1,0,0,2]
  , p "old-time" [1,0,0,5]
  , p "pretty" [1,0,1,1]
  , p "process" [1,0,1,3]
--  , p "random" [1,0,0,2] -- will not be shipped starting from ghc-7.2
--  , p "syb" [0,1,0,2] -- not distributed with ghc-7
  , p "template-haskell" [2,4,0,1]
--  , p "time" [1,1,4] package is upgradeable
  , p "unix" [2,4,0,2]
--  , p "utf8-string" [0,3,4] package is upgradeable
  ]

ghc6122_pkgs :: [PackageIdentifier]
ghc6122_pkgs = 
  [ p "array" [0,3,0,0]
  , p "base" [3,0,3,2]
  , p "base" [4,2,0,1]
  , p "bytestring" [0,9,1,6]
--  , p "Cabal" [1,8,0,4]  package is upgradeable
  , p "containers" [0,3,0,0]
  , p "directory" [1,0,1,1]
--  , p "extensible-exceptions" [0,1,1,1] -- package is upgradeable, stopped shipping in 7.6
  , p "filepath" [1,1,0,4]
  , p "haskell98" [1,0,1,1]
  , p "hpc" [0,5,0,5]
  , p "integer-gmp" [0,2,0,1]
  , p "integer-simple" [0,1,0,0]
  , p "old-locale" [1,0,0,2]
  , p "old-time" [1,0,0,4]
  , p "pretty" [1,0,1,1]
  , p "process" [1,0,1,2]
--  , p "random" [1,0,0,2] -- will not be shipped starting from ghc-7.2
--  , p "syb" [0,1,0,2] -- not distributed with ghc-7
  , p "template-haskell" [2,4,0,1]
--  , p "time" [1,1,4] package is upgradeable
  , p "unix" [2,4,0,1]
--  , p "utf8-string" [0,3,4] package is upgradeable
  ]

ghc6121_pkgs :: [PackageIdentifier]
ghc6121_pkgs =
  [ p "array" [0,3,0,0]
  , p "base" [3,0,3,2]
  , p "base" [4,2,0,0]
  , p "bytestring" [0,9,1,5]
--  , p "Cabal" [1,8,0,2]  package is upgradeable
  , p "containers" [0,3,0,0]
  , p "directory" [1,0,1,0]
--  , p "extensible-exceptions" [0,1,1,1] -- package is upgradeable, stopped shipping in 7.6
  , p "filepath" [1,1,0,3]
  , p "haskell98" [1,0,1,1]
  , p "hpc" [0,5,0,4]
  , p "integer-gmp" [0,2,0,0]
  , p "integer-simple" [0,1,0,0]
  , p "old-locale" [1,0,0,2]
  , p "old-time" [1,0,0,3]
  , p "pretty" [1,0,1,1]
  , p "process" [1,0,1,2]
--  , p "random" [1,0,0,2] -- will not be shipped starting from ghc-7.2
--  , p "syb" [0,1,0,2] -- not distributed with ghc-7
  , p "template-haskell" [2,4,0,0]
--  , p "time" [1,1,4] package is upgradeable
  , p "unix" [2,4,0,0]
--  , p "utf8-string" [0,3,4] package is upgradeable
  ]

ghc6104_pkgs :: [PackageIdentifier]
ghc6104_pkgs =
  [ p "array" [0,2,0,0]
  , p "base" [3,0,3,1]
  , p "base" [4,1,0,0]
  , p "bytestring" [0,9,1,4]
--  , p "Cabal" [1,6,0,3] package is upgradeable
  , p "containers" [0,2,0,1 ]
  , p "directory" [1,0,0,3]
--  , p "extensible-exceptions" [0,1,1,0] -- package is upgradeable, stopped shipping in 7.6
  , p "filepath" [1,1,0,2]
  , p "haskell98" [1,0,1,0]
  , p "hpc" [0,5,0,3]
  , p "old-locale" [1,0,0,1]
  , p "old-time" [1,0,0,2]
  , p "packedstring" [0,1,0,1]
  , p "pretty" [1,0,1,0]
  , p "process" [1,0,1,1]
--  , p "random" [1,0,0,1] -- will not be shipped starting from ghc-7.2
--  , p "syb" [0,1,0,1] -- not distributed with ghc-7
  , p "template-haskell" [2,3,0,1]
--  , p "time" [1,1,4] package is upgradeable
  , p "unix" [2,3,2,0]
  ]

p :: String -> [Int] -> PackageIdentifier
p pn vs = PackageIdentifier (PackageName pn) (Version vs [])
