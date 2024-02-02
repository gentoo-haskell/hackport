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
    [ ghc902, ghc924, ghc925, ghc926, ghc927, ghc928, ghc945, ghc946, ghc947
    , ghc948, ghc962, ghc963, ghc964
    ]

-- | Maybe determine the appropriate 'Cabal.Version' of the @Cabal@ package
-- from a given @GHC@ version.
--
-- >>> cabalFromGHC [9,2,7]
-- Just (mkVersion [3,6,3,0])
-- >>> cabalFromGHC [9,9,9,9]
-- Nothing
cabalFromGHC :: [Int] -> Maybe Cabal.Version
cabalFromGHC ver = lookup ver table
  where
  table = [ ([9,0,2], Cabal.mkVersion [3,4,1,0])
          , ([9,2,4], Cabal.mkVersion [3,6,3,0])
          , ([9,2,6], Cabal.mkVersion [3,6,3,0])
          , ([9,2,7], Cabal.mkVersion [3,6,3,0])
          , ([9,2,8], Cabal.mkVersion [3,6,3,0])
          , ([9,4,5], Cabal.mkVersion [3,8,1,0])
          , ([9,4,6], Cabal.mkVersion [3,8,1,0])
          , ([9,4,7], Cabal.mkVersion [3,8,1,0])
          , ([9,4,8], Cabal.mkVersion [3,8,1,0])
          , ([9,6,2], Cabal.mkVersion [3,10,1,0])
          , ([9,6,3], Cabal.mkVersion [3,10,1,0])
          , ([9,6,4], Cabal.mkVersion [3,10,1,0])
          ]

platform :: Platform
platform = Platform X86_64 Linux

-- | Is the package a core dependency of a specific version of @GHC@?
--
-- >>> packageIsCore (mkIndex [9,2,7] ghc927_pkgs) (Cabal.mkPackageName "binary")
-- True
-- >>> :{
--   let idx = mkIndex [9,2,7] ghc927_pkgs
--   in  all (== True) $ packageIsCore idx <$> packageNamesFromPackageIndex idx
-- :}
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
--
-- This takes the version of GHC as the first parameter, and uses it to
-- automatically add @ghc-boot@, @ghc-boot-th@, @ghc-heap@, and @ghci@ (all of
-- which share the same version number as GHC).
mkIndex :: [Int] -> [Cabal.PackageIdentifier] -> InstalledPackageIndex
mkIndex ghcVer pids = fromList
      [ emptyInstalledPackageInfo
          { sourcePackageId = pindex
          , exposed = True
          }
      | pindex@(Cabal.PackageIdentifier _name _version) <- pids' ]
  where
    pids' =
        p "ghc-boot" ghcVer
      : p "ghc-boot-th" ghcVer
      : p "ghc-heap" ghcVer
      : p "ghci" ghcVer
      : pids

packageNamesFromPackageIndex :: InstalledPackageIndex -> [Cabal.PackageName]
packageNamesFromPackageIndex pix = nub $ map fst $ allPackagesByName pix

ghc :: [Int] -> DC.CompilerInfo
ghc nrs = DC.unknownCompilerInfo c_id DC.NoAbiTag
    where c_id = CompilerId GHC (mkVersion nrs)

-- | Convenience function to combine the outputs of 'mkIndex' and 'ghc'
mkInfoIndex
  :: [Int]
  -> [Cabal.PackageIdentifier]
  -> (DC.CompilerInfo, InstalledPackageIndex)
mkInfoIndex ghcVer pids = (ghc ghcVer, mkIndex ghcVer pids)

ghc964 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc964 = mkInfoIndex [9,6,4] ghc964_pkgs

ghc963 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc963 = mkInfoIndex [9,6,3] ghc963_pkgs

ghc962 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc962 = mkInfoIndex [9,6,2] ghc962_pkgs

ghc948 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc948 = mkInfoIndex [9,4,8] ghc948_pkgs

ghc947 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc947 = mkInfoIndex [9,4,7] ghc947_pkgs

ghc946 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc946 = mkInfoIndex [9,4,6] ghc946_pkgs

ghc945 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc945 = mkInfoIndex [9,4,5] ghc945_pkgs

ghc928 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc928 = mkInfoIndex [9,2,8] ghc927_pkgs -- not a mistake: identical to 9.2.7

ghc927 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc927 = mkInfoIndex [9,2,7] ghc927_pkgs

ghc926 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc926 = mkInfoIndex [9,2,6] ghc926_pkgs

ghc925 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc925 = mkInfoIndex [9,2,5] ghc925_pkgs

ghc924 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc924 = mkInfoIndex [9,2,4] ghc924_pkgs

ghc902 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc902 = mkInfoIndex [9,0,2] ghc902_pkgs

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
ghc964_pkgs :: [Cabal.PackageIdentifier]
ghc964_pkgs =
  [ p "array" [0,5,5,0]
  , p "base" [4,18,2,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,3]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,1]
  , p "directory" [1,3,8,1]
  , p "exceptions" [0,10,7]
  , p "filepath" [1,4,200,4]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,10,0]
  , p "hpc" [0,6,2,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,17,0]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0]
  , p "terminfo" [0,4,1,6]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,4,0]
  ]

ghc963_pkgs :: [Cabal.PackageIdentifier]
ghc963_pkgs =
  [ p "array" [0,5,5,0]
  , p "base" [4,18,1,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,2]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,1]
  , p "directory" [1,3,8,1]
  , p "exceptions" [0,10,7]
  , p "filepath" [1,4,100,4]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,10,0]
  , p "hpc" [0,6,2,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,17,0]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0]
  , p "terminfo" [0,4,1,6]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,1,0]
  ]

ghc962_pkgs :: [Cabal.PackageIdentifier]
ghc962_pkgs =
  [ p "array" [0,5,5,0]
  , p "base" [4,18,0,0]
  , p "binary" [0,8,9,1] -- used by libghc
  , p "bytestring" [0,11,5,1] -- MUST BE PATCHED on ghc-9.6.2
--  , p "Cabal" [3,6,3,0]  package is upgradeable
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,1] -- used by time
  , p "directory" [1,3,8,1]
  , p "exceptions" [0,10,7] -- used by libghc
  , p "filepath" [1,4,100,1]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,10,0]
--  , p "haskeline" [0,8,2]  package is upgradeable
  , p "hpc" [0,6,2,0] -- used by libghc
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]  -- used by exceptions
--   , p "parsec" [3,1,15,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,17,0]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0] -- used by libghc
  , p "terminfo" [0,4,1,6] -- used by libghc
--   , p "text" [1,2,5,0] -- package is upgradeable
  , p "time" [1,12,2] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,6,1,0] -- used by libghc
  , p "unix" [2,8,1,0]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc948_pkgs :: [Cabal.PackageIdentifier]
ghc948_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,17,2,1]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,3]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,0]
  , p "directory" [1,3,7,1]
  , p "exceptions" [0,10,5]
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,9,1]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,18,0]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,19,0,0]
  , p "terminfo" [0,4,1,5]
  , p "time" [1,12,2]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,3]
  ]

ghc947_pkgs :: [Cabal.PackageIdentifier]
ghc947_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,17,2,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,2]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,0]
  , p "directory" [1,3,7,1]
  , p "exceptions" [0,10,5]
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,9,1]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,17,0]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,19,0,0]
  , p "terminfo" [0,4,1,5]
  , p "time" [1,12,2]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,3]
  ]

ghc946_pkgs :: [Cabal.PackageIdentifier]
ghc946_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,17,2,0]
  , p "binary" [0,8,9,1] -- used by libghc
  , p "bytestring" [0,11,5,1]
--  , p "Cabal" [3,6,3,0]  package is upgradeable
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,0] -- used by time
  , p "directory" [1,3,7,1]
  , p "exceptions" [0,10,5] -- used by libghc
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,9,1]
--  , p "haskeline" [0,8,2]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]  -- used by exceptions
--   , p "parsec" [3,1,15,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,17,0]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,19,0,0] -- used by libghc
  , p "terminfo" [0,4,1,5] -- used by libghc
--   , p "text" [1,2,5,0] -- package is upgradeable
  , p "time" [1,12,2] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,3]
--  , p "xhtml" [3000,2,2,1]
  ]


ghc945_pkgs :: [Cabal.PackageIdentifier]
ghc945_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,17,1,0]
  , p "binary" [0,8,9,1] -- used by libghc
  , p "bytestring" [0,11,4,0]
--  , p "Cabal" [3,6,3,0]  package is upgradeable
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,0] -- used by time
  , p "directory" [1,3,7,1]
  , p "exceptions" [0,10,5] -- used by libghc
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,9,0]
--  , p "haskeline" [0,8,2]  package is upgradeable
  , p "hpc" [0,6,1,0] -- used by libghc
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]  -- used by exceptions
--   , p "parsec" [3,1,15,0]  -- package is upgradeable
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,19,0,0] -- used by libghc
  , p "terminfo" [0,4,1,5] -- used by libghc
--   , p "text" [1,2,5,0] -- package is upgradeable
  , p "time" [1,12,2] -- used by unix, directory, hpc, ghc. unsafe to upgrade
  , p "transformers" [0,5,6,2] -- used by libghc
  , p "unix" [2,7,3]
--  , p "xhtml" [3000,2,2,1]
  ]

ghc927_pkgs :: [Cabal.PackageIdentifier]
ghc927_pkgs =
  [ p "array" [0,5,4,0]
  , p "base" [4,16,4,0]
  , p "binary" [0,8,9,0] -- used by libghc
  , p "bytestring" [0,11,4,0]
--  , p "Cabal" [3,6,3,0]  package is upgradeable
  , p "containers" [0,6,5,1]
  , p "deepseq" [1,4,6,1] -- used by time
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4] -- used by libghc
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,2]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
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
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
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
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
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
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
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
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,7,0]
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

p :: String -> [Int] -> Cabal.PackageIdentifier
p pn vs = Cabal.PackageIdentifier (Cabal.mkPackageName pn) (mkVersion vs)
