{-|
Module      : Portage.GHCCore
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Guess the appropriate GHC version from packages depended upon.
-}
module Portage.GHCCore
        ( minimumGHCVersionToBuildPackage
        , cabalFromGHC
        , defaultComponentRequestedSpec
        , finalizePD
        , platform
        , dependencySatisfiable
        -- hspec exports
        , packageIsCoreInAnyGHC
        ) where

import qualified Distribution.Compiler as DC
import qualified Distribution.Package as Cabal
import qualified Distribution.Version as Cabal
import Distribution.Version
import Distribution.Simple.PackageIndex
import Distribution.InstalledPackageInfo as IPI

import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Compiler (CompilerId(..), CompilerFlavor(GHC))
import Distribution.System
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)

import Distribution.Pretty (prettyShow)

import Data.Maybe
import Data.List ( nub )

import Debug.Trace

-- | Try each @GHC@ version in the specified order, from left to right.
-- The first @GHC@ version in this list is a minimum default.
ghcs :: [(DC.CompilerInfo, InstalledPackageIndex)]
ghcs =
    [ ghc881, ghc883, ghc884, ghc8101, ghc8104, ghc8106, ghc902, ghc924
    , ghc925, ghc926
    ]

-- | Maybe determine the appropriate 'Cabal.Version' of the @Cabal@ package
-- from a given @GHC@ version.
--
-- >>> cabalFromGHC [8,8,3]
-- Just (mkVersion [3,0,1,0])
-- >>> cabalFromGHC [9,9,9,9]
-- Nothing
cabalFromGHC :: [Int] -> Maybe Cabal.Version
cabalFromGHC ver = lookup ver table
  where
  table = [ ([8,8,1],  Cabal.mkVersion [3,0,0,0])
          , ([8,8,3],  Cabal.mkVersion [3,0,1,0])
          , ([8,8,4],  Cabal.mkVersion [3,0,1,0])
          , ([8,10,1], Cabal.mkVersion [3,2,0,0])
          , ([8,10,4], Cabal.mkVersion [3,2,1,0])
          , ([8,10,6], Cabal.mkVersion [3,2,1,0])
          , ([9,0,2], Cabal.mkVersion [3,4,1,0])
          , ([9,2,4], Cabal.mkVersion [3,6,3,0])
          , ([9,2,6], Cabal.mkVersion [3,6,3,0])
          ]

platform :: Platform
platform = Platform X86_64 Linux

-- | Is the package a core dependency of a specific version of @GHC@?
--
-- >>> packageIsCore (mkIndex ghc883_pkgs) (Cabal.mkPackageName "binary")
-- True
-- >>> all (== True) ((packageIsCore (mkIndex ghc883_pkgs)) <$> (packageNamesFromPackageIndex (mkIndex ghc883_pkgs)))
-- True
packageIsCore :: InstalledPackageIndex -> Cabal.PackageName -> Bool
packageIsCore index pn = not . null $ lookupPackageName index pn

-- | Is the package a core dependency of any version of @GHC@?
-- >>> packageIsCoreInAnyGHC (Cabal.mkPackageName "array")
-- True
packageIsCoreInAnyGHC :: Cabal.PackageName -> Bool
packageIsCoreInAnyGHC pn = any (flip packageIsCore pn) (map snd ghcs)

-- | Check if a dependency is satisfiable given a 'PackageIndex'
-- representing the core packages in a GHC version.
-- Packages that are not core will always be accepted, packages that are
-- core in any ghc must be satisfied by the 'PackageIndex'.
dependencySatisfiable :: InstalledPackageIndex -> Cabal.Dependency -> Bool
dependencySatisfiable pindex dep@(Cabal.Dependency pn _rang _lib)
  | Cabal.unPackageName pn == "Win32" = False -- only exists on windows, not in linux
  | not . null $ lookupDependency pindex (Cabal.depPkgName dep) (Cabal.depVerRange dep) = True -- the package index satisfies the dep
  | packageIsCoreInAnyGHC pn = False -- some other ghcs support the dependency
  | otherwise = True -- the dep is not related with core packages, accept the dep

packageBuildableWithGHCVersion
  :: GenericPackageDescription
  -> FlagAssignment
  -> (DC.CompilerInfo, InstalledPackageIndex)
  -> Either [Cabal.Dependency] (PackageDescription, FlagAssignment)
packageBuildableWithGHCVersion pkg user_specified_fas (compiler_info, pkgIndex) = trace_failure $
  finalizePD user_specified_fas defaultComponentRequestedSpec (dependencySatisfiable pkgIndex) platform compiler_info [] pkg
    where trace_failure v = case v of
              (Left deps) -> trace (unwords ["rejecting dep:" , show_compiler compiler_info
                                            , "as", show_deps deps
                                            , "were not found."
                                            ]
                                   ) v
              _           -> trace (unwords ["accepting dep:" , show_compiler compiler_info
                                            ]
                                   ) v
          show_deps = show . map prettyShow
          show_compiler (DC.CompilerInfo { DC.compilerInfoId = CompilerId GHC v }) = "ghc-" ++ prettyShow v
          show_compiler c = show c

-- | Given a 'GenericPackageDescription' it returns the miminum GHC version
-- to build a package, and a list of core packages to that GHC version.
minimumGHCVersionToBuildPackage :: GenericPackageDescription -> FlagAssignment -> Maybe ( DC.CompilerInfo
                                                                                        , [Cabal.PackageName]
                                                                                        , PackageDescription
                                                                                        , FlagAssignment
                                                                                        , InstalledPackageIndex)
minimumGHCVersionToBuildPackage gpd user_specified_fas =
  listToMaybe [ (cinfo, packageNamesFromPackageIndex pix, pkg_desc, picked_flags, pix)
              | g@(cinfo, pix) <- ghcs
              , Right (pkg_desc, picked_flags) <- return (packageBuildableWithGHCVersion gpd user_specified_fas g)]

-- | Create an 'InstalledPackageIndex' from a ['Cabal.PackageIdentifier'].
-- This is used to generate an index of core @GHC@ packages from the provided
-- ['Cabal.PackageIdentifier'] functions, e.g. 'ghc883_pkgs'.
mkIndex :: [Cabal.PackageIdentifier] -> InstalledPackageIndex
mkIndex pids = fromList
  [ emptyInstalledPackageInfo
      { sourcePackageId = pindex
      , exposed = True
      }
  | pindex@(Cabal.PackageIdentifier _name _version) <- pids ]

packageNamesFromPackageIndex :: InstalledPackageIndex -> [Cabal.PackageName]
packageNamesFromPackageIndex pix = nub $ map fst $ allPackagesByName pix

ghc :: [Int] -> DC.CompilerInfo
ghc nrs = DC.unknownCompilerInfo c_id DC.NoAbiTag
    where c_id = CompilerId GHC (mkVersion nrs)

ghc926 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc926 = (ghc [9,2,6], mkIndex ghc926_pkgs)

ghc925 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc925 = (ghc [9,2,5], mkIndex ghc925_pkgs)

ghc924 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc924 = (ghc [9,2,4], mkIndex ghc924_pkgs)

ghc902 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc902 = (ghc [9,0,2], mkIndex ghc902_pkgs)

ghc8106 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc8106 = (ghc [8,10,6], mkIndex ghc8106_pkgs)

ghc8104 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc8104 = (ghc [8,10,4], mkIndex ghc8104_pkgs)

ghc8101 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc8101 = (ghc [8,10,1], mkIndex ghc8101_pkgs)

ghc884 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc884 = (ghc [8,8,4], mkIndex ghc884_pkgs)

ghc883 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc883 = (ghc [8,8,3], mkIndex ghc883_pkgs)

ghc881 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc881 = (ghc [8,8,1], mkIndex ghc881_pkgs)

-- | Non-upgradeable core packages
-- Sources:
--  * release notes
--      example: https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/8.6.5-notes.html
--  * our binary tarballs (package.conf.d.initial subdir)
--  * ancient: http://haskell.org/haskellwiki/Libraries_released_with_GHC
--  * https://github.com/gentoo-haskell/gentoo-haskell/issues/1386
--  * https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.0.2-release/compiler/ghc.cabal.in#L60-77
--  * https://flora.pm/packages/%40hackage/ghc/9.0.2/dependencies
--  * https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history
ghc926_pkgs :: [Cabal.PackageIdentifier]
ghc926_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,16,4,0]
  , p "binary" [0,8,9,0] -- used by libghc
  , p "bytestring" [0,11,4,0]
--  , p "Cabal" [3,6,3,0]  package is upgradeable
  , p "containers" [0,6,5,1]
  , p "deepseq" [1,4,6,1] -- used by time
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4] -- used by libghc
  , p "filepath" [1,4,2,1]
  , p "ghc-bignum" [1,2]
  , p "ghc-boot" [9,2,6]
  , p "ghc-boot-th" [9,2,6]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
  , p "ghc-heap" [9,2,6]
  , p "ghci" [9,2,6]
--  , p "haskeline" [0,8,2]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]  -- used by exceptions
--   , p "parsec" [3,1,15,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "stm" [2,5,0,2]
  , p "template-haskell" [2,18,0,0] -- used by libghc
  , p "terminfo" [0,4,1,5] -- used by libghc
--   , p "text" [1,2,5,0] -- package is upgradeable
  , p "time" [1,11,1,1] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc925_pkgs :: [Cabal.PackageIdentifier]
ghc925_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,16,4,0]
  , p "binary" [0,8,9,0] -- used by libghc
  , p "bytestring" [0,11,3,1]
--  , p "Cabal" [3,6,3,0]  package is upgradeable
  , p "containers" [0,6,5,1]
  , p "deepseq" [1,4,6,1] -- used by time
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4] -- used by libghc
  , p "filepath" [1,4,2,1]
  , p "ghc-bignum" [1,2]
  , p "ghc-boot" [9,2,5]
  , p "ghc-boot-th" [9,2,5]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
  , p "ghc-heap" [9,2,5]
  , p "ghci" [9,2,5]
--  , p "haskeline" [0,8,2]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]  -- used by exceptions
--   , p "parsec" [3,1,15,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "stm" [2,5,0,2]
  , p "template-haskell" [2,18,0,0] -- used by libghc
  , p "terminfo" [0,4,1,5] -- used by libghc
--   , p "text" [1,2,5,0] -- package is upgradeable
  , p "time" [1,11,1,1] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc924_pkgs :: [Cabal.PackageIdentifier]
ghc924_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,16,3,0]
  , p "binary" [0,8,9,0] -- used by libghc
  , p "bytestring" [0,11,3,1]
--  , p "Cabal" [3,6,3,0]  package is upgradeable
  , p "containers" [0,6,5,1]
  , p "deepseq" [1,4,6,1] -- used by time
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4] -- used by libghc
  , p "filepath" [1,4,2,1]
  , p "ghc-bignum" [1,2]
  , p "ghc-boot" [9,2,4]
  , p "ghc-boot-th" [9,2,4]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
  , p "ghc-heap" [9,2,4]
  , p "ghci" [9,2,4]
--  , p "haskeline" [0,8,2]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]  -- used by exceptions
--   , p "parsec" [3,1,15,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "stm" [2,5,0,2]
  , p "template-haskell" [2,18,0,0] -- used by libghc
  , p "terminfo" [0,4,1,5] -- used by libghc
--   , p "text" [1,2,5,0] -- package is upgradeable
  , p "time" [1,11,1,1] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc902_pkgs :: [Cabal.PackageIdentifier]
ghc902_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,15,1,0]
  , p "binary" [0,8,8,0] -- used by libghc
  , p "bytestring" [0,10,12,1]
--  , p "Cabal" [3,4,1,0]  package is upgradeable
  , p "containers" [0,6,4,1]
  , p "deepseq" [1,4,5,0] -- used by time
  , p "directory" [1,3,6,2]
  , p "filepath" [1,4,2,1]
  , p "exceptions" [0,10,4] -- used by libghc
  , p "ghc-bignum" [1,1]
  , p "ghc-boot" [9,0,2]
  , p "ghc-boot-th" [9,0,2]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,7,0]
  , p "ghc-heap" [9,0,2]
  , p "ghci" [9,0,2]
--  , p "haskeline" [0,8,2]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]  -- used by exceptions
--   , p "parsec" [3,1,14,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "stm" [2,5,0,0]
  , p "template-haskell" [2,17,0,0] -- used by libghc
  , p "terminfo" [0,4,1,5] -- used by libghc
--   , p "text" [1,2,5,0] -- package is upgradeable
  , p "time" [1,9,3,0] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc8106_pkgs :: [Cabal.PackageIdentifier]
ghc8106_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,14,1,0]
  , p "binary" [0,8,8,0] -- used by libghc
  , p "bytestring" [0,10,12,0]
--  , p "Cabal" [3,2,1,0]  package is upgradeable
  , p "containers" [0,6,2,1]
  , p "deepseq" [1,4,4,0] -- used by time
  , p "directory" [1,3,6,0]
  , p "filepath" [1,4,2,1]
  , p "exceptions" [0,10,4] -- used by libghc in ghc-9.0.2
  , p "ghc-boot" [8,10,4]
  , p "ghc-boot-th" [8,10,4]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,6,1,0]
  , p "ghc-heap" [8,10,4]
  , p "ghci" [8,10,4]
--  , p "haskeline" [0,8,0,1]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,0,3,0]
  , p "mtl" [2,2,2]  -- used by exceptions in ghc-9.0.2
--   , p "parsec" [3,1,14,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "stm" [2,5,0,0]
  , p "template-haskell" [2,16,0,0] -- used by libghc
  , p "terminfo" [0,4,1,4] -- used by libghc in ghc-9.0.2
--   , p "text" [1,2,4,1] -- package is upgradeable
  , p "time" [1,9,3,0] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc8104_pkgs :: [Cabal.PackageIdentifier]
ghc8104_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,14,1,0]
  , p "binary" [0,8,8,0] -- used by libghc
  , p "bytestring" [0,10,12,0]
--  , p "Cabal" [3,2,1,0]  package is upgradeable
  , p "containers" [0,6,2,1]
  , p "deepseq" [1,4,4,0] -- used by time
  , p "directory" [1,3,6,0]
  , p "filepath" [1,4,2,1]
  , p "exceptions" [0,10,4] -- used by libghc in ghc-9.0.2
  , p "ghc-boot" [8,10,4]
  , p "ghc-boot-th" [8,10,4]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,6,1,0]
  , p "ghc-heap" [8,10,4]
  , p "ghci" [8,10,4]
--  , p "haskeline" [0,8,0,1]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,0,3,0]
  , p "mtl" [2,2,2]  -- used by exceptions in ghc-9.0.2
--   , p "parsec" [3,1,14,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,9,0]
  , p "stm" [2,5,0,0]
  , p "template-haskell" [2,16,0,0] -- used by libghc
  , p "terminfo" [0,4,1,4] -- used by libghc in ghc-9.0.2
--   , p "text" [1,2,4,1] -- package is upgradeable
  , p "time" [1,9,3,0] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc8101_pkgs :: [Cabal.PackageIdentifier]
ghc8101_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,14,0,0]
  , p "binary" [0,8,8,0] -- used by libghc
  , p "bytestring" [0,10,10,0]
--  , p "Cabal" [3,2,0,0]  package is upgradeable
  , p "containers" [0,6,2,1]
  , p "deepseq" [1,4,4,0] -- used by time
  , p "directory" [1,3,6,0]
  , p "filepath" [1,4,2,1]
  , p "exceptions" [0,10,4] -- used by libghc in ghc-9.0.2
  , p "ghc-boot" [8,10,1]
  , p "ghc-boot-th" [8,10,1]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,6,1,0]
  , p "ghc-heap" [8,10,1]
  , p "ghci" [8,10,1]
--  , p "haskeline" [0,8,0,0]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,0,3,0]
  , p "mtl" [2,2,2]  -- used by libghc in ghc-9.0.2 
--   , p "parsec" [3,1,14,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,8,2]
  , p "stm" [2,5,0,0]
  , p "template-haskell" [2,16,0,0] -- used by libghc
  , p "terminfo" [0,4,1,4]  -- used by libghc in ghc-9.0.2
--   , p "text" [1,2,3,2]  -- package is upgradeable
  , p "time" [1,9,3,0] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc884_pkgs :: [Cabal.PackageIdentifier]
ghc884_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,13,0,0]
  , p "binary" [0,8,7,0] -- used by libghc
  , p "bytestring" [0,10,10,1]
--  , p "Cabal" [3,0,1,0]  package is upgradeable
  , p "containers" [0,6,2,1]
  , p "deepseq" [1,4,4,0] -- used by time
  , p "directory" [1,3,6,0]
  , p "filepath" [1,4,2,1]
  , p "ghc-boot" [8,8,4]
  , p "ghc-boot-th" [8,8,4]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,5,3,0]
  , p "ghci" [8,8,4]
--  , p "haskeline" [0,7,5,0]  package is upgradeable
  , p "hpc" [0,6,0,3] -- used by libghc
  , p "integer-gmp" [1,0,2,0]
  , p "mtl" [2,2,2]  -- used by exceptions in ghc-9.0.2
--   , p "parsec" [3,1,14,0] -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,9,0]
  , p "stm" [2,5,0,0]
  , p "template-haskell" [2,15,0,0] -- used by libghc
  , p "terminfo" [0,4,1,4] -- used by libghc in ghc-9.0.2
--   , p "text" [1,2,4,0] -- package is upgradeable
  , p "time" [1,9,3,0] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc883_pkgs :: [Cabal.PackageIdentifier]
ghc883_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,13,0,0]
  , p "binary" [0,8,7,0] -- used by libghc
  , p "bytestring" [0,10,10,0]
--  , p "Cabal" [3,0,1,0]  package is upgradeable
  , p "containers" [0,6,2,1]
  , p "deepseq" [1,4,4,0] -- used by time
  , p "directory" [1,3,6,0]
  , p "filepath" [1,4,2,1]
  , p "ghc-boot" [8,8,3]
  , p "ghc-boot-th" [8,8,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,5,3,0]
  , p "ghci" [8,8,3]
--  , p "haskeline" [0,7,5,0]  package is upgradeable
  , p "hpc" [0,6,0,3] -- used by libghc
  , p "integer-gmp" [1,0,2,0]
  , p "mtl" [2,2,2] -- used by exceptions in ghc-9.0.2
--   , p "parsec" [3,1,14,0] -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,8,0]
  , p "stm" [2,5,0,0]
  , p "template-haskell" [2,15,0,0] -- used by libghc
  , p "terminfo" [0,4,1,4] -- used by libghc in ghc-9.0.2
--   , p "text" [1,2,4,0] -- package is upgradeable
  , p "time" [1,9,3,0] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc881_pkgs :: [Cabal.PackageIdentifier]
ghc881_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,13,0,0]
  , p "binary" [0,8,7,0] -- used by libghc
  , p "bytestring" [0,10,9,0]
--  , p "Cabal" [3,0,0,0]  package is upgradeable
  , p "containers" [0,6,2,1]
  , p "deepseq" [1,4,4,0] -- used by time
  , p "directory" [1,3,3,2]
  , p "filepath" [1,4,2,1]
  , p "ghc-boot" [8,8,1]
  , p "ghc-boot-th" [8,8,1]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,5,3,0]
  , p "ghci" [8,8,1]
--  , p "haskeline" [0,7,4,3]  package is upgradeable
  , p "hpc" [0,6,0,3] -- used by libghc
  , p "integer-gmp" [1,0,2,0]
  , p "mtl" [2,2,2] -- used by exceptions in ghc-9.0.2
--   , p "parsec" [3,1,14,0] -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,5,1]
  , p "stm" [2,5,0,0]
  , p "template-haskell" [2,15,0,0] -- used by libghc
  , p "terminfo" [0,4,1,4] -- used by libghc in ghc-9.0.2
--   , p "text" [1,2,4,0] -- package is upgradeable
  , p "time" [1,9,3,0] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,2,2]
--  , p "xhtml" [3000,2,2,1]
  ]

p :: String -> [Int] -> Cabal.PackageIdentifier
p pn vs = Cabal.PackageIdentifier (Cabal.mkPackageName pn) (mkVersion vs)
