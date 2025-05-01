{-# LANGUAGE OverloadedStrings #-} -- For Cabal.PackageName

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
import qualified Data.Set as S

import Debug.Trace

-- | Try each @GHC@ version in the specified order, from left to right.
-- The first @GHC@ version in this list is a minimum default.
ghcs :: [(DC.CompilerInfo, InstalledPackageIndex)]
ghcs =
    [ ghc902
    , ghc924, ghc925, ghc926, ghc927, ghc928
    , ghc945, ghc946, ghc947, ghc948
    , ghc962, ghc963, ghc964, ghc965, ghc966, ghc967
    , ghc982, ghc983, ghc984
    , ghc9101
    , ghc9121, ghc9122
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
          , ([9,6,5], Cabal.mkVersion [3,10,3,0])
          , ([9,6,6], Cabal.mkVersion [3,10,3,0])
          , ([9,6,7], Cabal.mkVersion [3,10,3,0])
          , ([9,8,2], Cabal.mkVersion [3,10,3,0]) -- bumped via BUMP_LIBRARIES
          , ([9,8,3], Cabal.mkVersion [3,10,3,0])
          , ([9,8,4], Cabal.mkVersion [3,10,3,0])
          , ([9,10,1], Cabal.mkVersion [3,12,0,0])
          , ([9,12,1], Cabal.mkVersion [3,14,1,1]) -- bumped via BUMP_LIBRARIES
          , ([9,12,2], Cabal.mkVersion [3,14,1,1]) -- bumped via BUMP_LIBRARIES
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
-- which share the same version number as GHC). In addition, it generates
-- what seems to be standard versioning for @ghc-experimental@ and @ghc-internal@.
-- A Default library number will only be applied if it is not already present
-- in @pids@.
mkIndex :: [Int] -> [Cabal.PackageIdentifier] -> InstalledPackageIndex
mkIndex ghcVer pids = fromList
      [ emptyInstalledPackageInfo
          { sourcePackageId = pindex
          , exposed = True
          }
      | pindex@(Cabal.PackageIdentifier _name _version) <- pids' ]
  where
    pids' = S.toAscList $ S.union
      ( S.fromList (filter filtUpgradeable pids) )
      -- These defaults will be ignored if they are already defined in 'pids'
      ( S.fromList
          [ p "ghc-boot" ghcVer
          , p "ghc-boot-th" ghcVer
          , p "ghc-heap" ghcVer
          , p "ghci" ghcVer
          , ghcExp "ghc-experimental" ghcVer
          , ghcExp "ghc-internal" ghcVer
          ] )

    -- | only include 'Cabal.PackageIdentifier's whose 'Cabal.packageName' does
    --   not match anything from 'upgradeablePkgs'
    filtUpgradeable :: Cabal.PackageIdentifier -> Bool
    filtUpgradeable pid =
        all (Cabal.packageName pid /=) upgradeablePkgs

    -- e.g. foo-9.1201.0 from: ghcExp "foo" [9,12,1]
    ghcExp :: String -> [Int] -> Cabal.PackageIdentifier
    ghcExp s (v:vs) = p s [v, read (concat (map pad vs)), 0]
        where pad i | i < 10 = "0" ++ show i
                    | otherwise = show i
    ghcExp s _ = p s [0]

-- | These bundled packages are "upgradeable" and should not be present in the
--   generated 'InstalledPackageIndex'. If they are, they will /not/ show up
--   in the generated list of dependencies (@RDEPEND@ and/or @DEPEND@), which
--   is undesireable (since this is where required version ranges are found).
--
--   See: <https://github.com/gentoo-haskell/gentoo-haskell/issues/1386>
upgradeablePkgs :: [Cabal.PackageName]
upgradeablePkgs =
    [ -- Upgradeable packages bundled with >=ghc-9.12
      "haddock-api"
    , "haddock-library"
      -- NOT upgradeable in ghc-9.12, but needs to be added to the ebuild for
      -- earlier versions of ghc.
    , "os-string"
      -- Upgradeable packages bundled with >=ghc-9.10
    , "ghc-platform"
    , "ghc-toolchain"
      -- Upgradeable packages bundled with >=ghc-9.4
    , "Cabal-syntax"
      -- The rest
    , "Cabal"
    , "haskeline"
    , "parsec"
    , "text"
    , "xhtml"
    ]


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

ghc9122 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc9122 = mkInfoIndex [9,12,2] ghc9122_pkgs

ghc9121 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc9121 = mkInfoIndex [9,12,1] ghc9121_pkgs

ghc9101 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc9101 = mkInfoIndex [9,10,1] ghc9101_pkgs

ghc984 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc984 = mkInfoIndex [9,8,4] ghc984_pkgs

ghc983 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc983 = mkInfoIndex [9,8,3] ghc983_pkgs

ghc982 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc982 = mkInfoIndex [9,8,2] ghc982_pkgs

ghc967 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc967 = mkInfoIndex [9,6,7] ghc967_pkgs

ghc966 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc966 = mkInfoIndex [9,6,6] ghc966_pkgs

ghc965 :: (DC.CompilerInfo, InstalledPackageIndex)
ghc965 = mkInfoIndex [9,6,5] ghc965_pkgs

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

-- | Core packages. Some packages are not included for simplicity (see 'mkIndex').
--
-- Sources:
--  * release notes
--      example: https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/8.6.5-notes.html
--  * our binary tarballs (package.conf.d.initial subdir)
--  * ancient: http://haskell.org/haskellwiki/Libraries_released_with_GHC
--  * https://github.com/gentoo-haskell/gentoo-haskell/issues/1386
--  * https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.0.2-release/compiler/ghc.cabal.in#L60-77
--  * https://flora.pm/packages/%40hackage/ghc/9.0.2/dependencies
--  * https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history
--  * @./scripts/scan-ghc-library-versions.hs@ in the gentoo-haskell tree
ghc9122_pkgs :: [Cabal.PackageIdentifier]
ghc9122_pkgs =
  [ p "Cabal-syntax" [3,14,1,0]
  , p "Cabal" [3,14,1,1] -- bumped via BUMP_LIBRARIES
  , p "array" [0,5,8,0]
  , p "base" [4,21,0,0]
  , p "binary" [0,8,9,3]
  , p "bytestring" [0,12,2,0]
  , p "containers" [0,7]
  , p "deepseq" [1,5,1,0]
  , p "directory" [1,3,9,0]
  , p "exceptions" [0,10,9]
  , p "file-io" [0,1,5]
  , p "filepath" [1,5,4,0]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,13,0]
  , p "ghc-platform" [0,1,0,0]
  , p "ghc-toolchain" [0,1,0,0]
  , p "haddock-api" [2,30,0]
  , p "haddock-library" [1,11,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,7,0,2]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "os-string" [2,0,7]
  , p "parsec" [3,1,18,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,25,0]
  , p "rts" [1,0,2]
  , p "semaphore-compat" [1,0,0]
  , p "stm" [2,5,3,1]
  , p "template-haskell" [2,23,0,0]
  , p "terminfo" [0,4,1,7]
  , p "text" [2,1,2]
  , p "time" [1,14]
  , p "transformers" [0,6,1,2]
  , p "unix" [2,8,6,0]
  , p "xhtml" [3000,2,2,1]
  ]

ghc9121_pkgs :: [Cabal.PackageIdentifier]
ghc9121_pkgs =
  [ p "Cabal-syntax" [3,14,1,0]
  , p "Cabal" [3,14,1,1] -- bumped via BUMP_LIBRARIES
  , p "array" [0,5,8,0]
  , p "base" [4,21,0,0]
  , p "binary" [0,8,9,2]
  , p "bytestring" [0,12,2,0]
  , p "containers" [0,7]
  , p "deepseq" [1,5,1,0]
  , p "directory" [1,3,9,0]
  , p "exceptions" [0,10,9]
  , p "file-io" [0,1,5]
  , p "filepath" [1,5,4,0]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,13,0]
  , p "ghc-platform" [0,1,0,0]
  , p "ghc-toolchain" [0,1,0,0]
  , p "haddock-api" [2,30,0]
  , p "haddock-library" [1,11,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,7,0,1]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "os-string" [2,0,7]
  , p "parsec" [3,1,17,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,25,0]
  , p "rts" [1,0,2]
  , p "semaphore-compat" [1,0,0]
  , p "stm" [2,5,3,1]
  , p "template-haskell" [2,23,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,1,2]
  , p "time" [1,14]
  , p "transformers" [0,6,1,2]
  , p "unix" [2,8,6,0]
  , p "xhtml" [3000,2,2,1]
  ]

ghc9101_pkgs :: [Cabal.PackageIdentifier]
ghc9101_pkgs =
  [ p "Cabal-syntax" [3,12,0,0]
  , p "Cabal" [3,12,0,0]
  , p "array" [0,5,8,0] -- bumped via BUMP_LIBRARIES
  , p "base" [4,20,0,0]
  , p "binary" [0,8,9,2]
  , p "bytestring" [0,12,1,0]
  , p "containers" [0,7]
  , p "deepseq" [1,5,1,0] -- bumped via BUMP_LIBRARIES
  , p "directory" [1,3,9,0] -- bumped via BUMP_LIBRARIES
  , p "exceptions" [0,10,7]
  , p "file-io" [0,1,5] -- added via hadrian patch and BUMP_LIBRARIES
  , p "filepath" [1,5,2,0]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-experimental" [0,1,0,0]
  , p "ghc-internal" [9,1001,0]
  , p "ghc-prim" [0,11,0]
  , p "ghc-platform" [0,1,0,0]
  , p "ghc-toolchain" [0,1,0,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,7,0,1]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "os-string" [2,0,7] -- bumped via BUMP_LIBRARIES
  , p "parsec" [3,1,17,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,25,0] -- bumped via BUMP_LIBRARIES
  , p "rts" [1,0,2]
  , p "semaphore-compat" [1,0,0]
  , p "stm" [2,5,3,1]
  , p "template-haskell" [2,22,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,1,1]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,1]
  , p "unix" [2,8,5,1]
  , p "xhtml" [3000,2,2,1]
  ]

ghc984_pkgs :: [Cabal.PackageIdentifier]
ghc984_pkgs =
  [ p "Cabal-syntax" [3,10,3,0]
  , p "Cabal" [3,10,3,0]
  , p "array" [0,5,8,0]
  , p "base" [4,19,2,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,12,1,0]
  , p "containers" [0,6,8]
  , p "deepseq" [1,5,1,0]
  , p "directory" [1,3,9,0] -- bumped via BUMP_LIBRARIES
  , p "exceptions" [0,10,7]
  , p "file-io" [0,1,5] -- added via hadrian patch and BUMP_LIBRARIES
  , p "filepath" [1,4,301,0]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,11,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,7,0,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,17,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,25,0]
  , p "rts" [1,0,2]
  , p "semaphore-compat" [1,0,0]
  , p "stm" [2,5,3,1]
  , p "template-haskell" [2,21,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,1,1]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,6,0]
  , p "xhtml" [3000,2,2,1]
  ]

ghc983_pkgs :: [Cabal.PackageIdentifier]
ghc983_pkgs =
  [ p "Cabal-syntax" [3,10,3,0]
  , p "Cabal" [3,10,3,0]
  , p "array" [0,5,8,0]
  , p "base" [4,19,2,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,12,1,0]
  , p "containers" [0,6,8]
  , p "deepseq" [1,5,1,0]
  , p "directory" [1,3,9,0] -- bumped via BUMP_LIBRARIES
  , p "exceptions" [0,10,7]
  , p "file-io" [0,1,5] -- added via hadrian patch and BUMP_LIBRARIES
  , p "filepath" [1,4,301,0] -- bumped via BUMP_LIBRARIES
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,11,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,7,0,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,17,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,25,0]
  , p "rts" [1,0,2]
  , p "semaphore-compat" [1,0,0]
  , p "stm" [2,5,3,1]
  , p "template-haskell" [2,21,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,1,1]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,6,0] -- bumped via BUMP_LIBRARIES
  , p "xhtml" [3000,2,2,1]
  ]

ghc982_pkgs :: [Cabal.PackageIdentifier]
ghc982_pkgs =
  [ p "Cabal-syntax" [3,10,3,0] -- bumped via BUMP_LIBRARIES
  , p "Cabal" [3,10,3,0] -- bumped via BUMP_LIBRARIES
  , p "array" [0,5,8,0] -- bumped via BUMP_LIBRARIES
  , p "base" [4,19,1,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,12,1,0]
  , p "containers" [0,6,8]
  , p "deepseq" [1,5,0,0]
  , p "directory" [1,3,9,0] -- bumped via BUMP_LIBRARIES
  , p "exceptions" [0,10,7]
  , p "file-io" [0,1,5] -- added via hadrian patch and BUMP_LIBRARIES
  , p "filepath" [1,4,301,0] -- bumped via BUMP_LIBRARIES
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,11,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,7,0,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,17,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,25,0] -- bumped via BUMP_LIBRARIES
  , p "rts" [1,0,2]
  , p "semaphore-compat" [1,0,0]
  , p "stm" [2,5,3,1] -- bumped via BUMP_LIBRARIES
  , p "template-haskell" [2,21,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,1,1]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,6,0] -- bumped via BUMP_LIBRARIES
  , p "xhtml" [3000,2,2,1]
  ]

ghc967_pkgs :: [Cabal.PackageIdentifier]
ghc967_pkgs =
  [ p "Cabal-syntax" [3,10,3,0]
  , p "Cabal" [3,10,3,0]
  , p "array" [0,5,8,0]
  , p "base" [4,18,3,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,4]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,1]
  , p "directory" [1,3,9,0] -- bumped via BUMP_LIBRARIES
  , p "exceptions" [0,10,7]
  , p "file-io" [0,1,5] -- added via hadrian patch and BUMP_LIBRARIES
  , p "filepath" [1,4,301,0]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,10,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,6,2,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,19,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,6,0]
  , p "xhtml" [3000,2,2,1]
  ]

ghc966_pkgs :: [Cabal.PackageIdentifier]
ghc966_pkgs =
  [ p "Cabal-syntax" [3,10,3,0]
  , p "Cabal" [3,10,3,0]
  , p "array" [0,5,6,0]
  , p "base" [4,18,2,1]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,3]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,1]
  , p "directory" [1,3,8,5]
  , p "exceptions" [0,10,7]
  , p "filepath" [1,4,300,1]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,10,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,6,2,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,19,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,4,0]
  , p "xhtml" [3000,2,2,1]
  ]


ghc965_pkgs :: [Cabal.PackageIdentifier]
ghc965_pkgs =
  [ p "Cabal-syntax" [3,10,3,0]
  , p "Cabal" [3,10,3,0]
  , p "array" [0,5,6,0]
  , p "base" [4,18,2,1]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,3]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,1]
  , p "directory" [1,3,8,4]
  , p "exceptions" [0,10,7]
  , p "filepath" [1,4,300,1]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,10,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,6,2,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,19,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,4,0]
  , p "xhtml" [3000,2,2,1]
  ]

ghc964_pkgs :: [Cabal.PackageIdentifier]
ghc964_pkgs =
  [ p "Cabal-syntax" [3,10,1,0]
  , p "Cabal" [3,10,1,0]
  , p "array" [0,5,6,0]
  , p "base" [4,18,2,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,3]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,1]
  , p "directory" [1,3,8,1]
  , p "exceptions" [0,10,7]
  , p "filepath" [1,4,200,1]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,10,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,6,2,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,18,0] -- bumped via BUMP_LIBRARIES
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,4,0]
  , p "xhtml" [3000,2,2,1]
  ]

ghc963_pkgs :: [Cabal.PackageIdentifier]
ghc963_pkgs =
  [ p "Cabal-syntax" [3,10,1,0]
  , p "Cabal" [3,10,1,0]
  , p "array" [0,5,5,0]
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
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,6,2,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,18,0] -- bumped via BUMP_LIBRARIES
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,1,0]
  , p "xhtml" [3000,2,2,1]
  ]

ghc962_pkgs :: [Cabal.PackageIdentifier]
ghc962_pkgs =
  [ p "Cabal-syntax" [3,10,1,0]
  , p "Cabal" [3,10,1,0]
  , p "array" [0,5,5,0]
  , p "base" [4,18,0,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,1] -- MUST BE PATCHED on ghc-9.6.2
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,1]
  , p "directory" [1,3,8,1]
  , p "exceptions" [0,10,7]
  , p "filepath" [1,4,100,1]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,10,0]
  , p "haskeline" [0,8,2,1]
  , p "hpc" [0,6,2,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,3,1]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,17,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,20,0,0]
  , p "terminfo" [0,4,1,6]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,6,1,0]
  , p "unix" [2,8,1,0]
  , p "xhtml" [3000,2,2,1]
  ]

ghc948_pkgs :: [Cabal.PackageIdentifier]
ghc948_pkgs =
  [ p "Cabal-syntax" [3,8,1,0]
  , p "Cabal" [3,8,1,0]
  , p "array" [0,5,4,0]
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
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,18,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,19,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,3]
  , p "xhtml" [3000,2,2,1]
  ]

ghc947_pkgs :: [Cabal.PackageIdentifier]
ghc947_pkgs =
  [ p "Cabal-syntax" [3,8,1,0]
  , p "Cabal" [3,8,1,0]
  , p "array" [0,5,4,0]
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
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,17,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,19,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,3]
  , p "xhtml" [3000,2,2,1]
  ]

ghc946_pkgs :: [Cabal.PackageIdentifier]
ghc946_pkgs =
  [ p "Cabal-syntax" [3,8,1,0]
  , p "Cabal" [3,8,1,0]
  , p "array" [0,5,4,0]
  , p "base" [4,17,2,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,5,1]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,0]
  , p "directory" [1,3,7,1]
  , p "exceptions" [0,10,5]
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,9,1]
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,17,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,19,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,3]
  , p "xhtml" [3000,2,2,1]
  ]

ghc945_pkgs :: [Cabal.PackageIdentifier]
ghc945_pkgs =
  [ p "Cabal-syntax" [3,8,1,0]
  , p "Cabal" [3,8,1,0]
  , p "array" [0,5,4,0]
  , p "base" [4,17,1,0]
  , p "binary" [0,8,9,1]
  , p "bytestring" [0,11,4,0]
  , p "containers" [0,6,7]
  , p "deepseq" [1,4,8,0]
  , p "directory" [1,3,7,1]
  , p "exceptions" [0,10,5]
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,3]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,9,0]
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,16,1]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,1,0]
  , p "template-haskell" [2,19,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [2,0,2]
  , p "time" [1,12,2]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,3]
  , p "xhtml" [3000,2,2,1]
  ]

ghc927_pkgs :: [Cabal.PackageIdentifier]
ghc927_pkgs =
  [ p "Cabal" [3,6,3,0]
  , p "array" [0,5,4,0]
  , p "base" [4,16,4,0]
  , p "binary" [0,8,9,0]
  , p "bytestring" [0,11,4,0]
  , p "containers" [0,6,5,1]
  , p "deepseq" [1,4,6,1]
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4]
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,2]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,15,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,0,2]
  , p "template-haskell" [2,18,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [1,2,5,0]
  , p "time" [1,11,1,1]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,2,2]
  , p "xhtml" [3000,2,2,1]
  ]

ghc926_pkgs :: [Cabal.PackageIdentifier]
ghc926_pkgs =
  [ p "Cabal" [3,6,3,0]
  , p "array" [0,5,4,0]
  , p "base" [4,16,4,0]
  , p "binary" [0,8,9,0]
  , p "bytestring" [0,11,4,0]
  , p "containers" [0,6,5,1]
  , p "deepseq" [1,4,6,1]
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4]
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,2]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,15,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,0,2]
  , p "template-haskell" [2,18,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [1,2,5,0]
  , p "time" [1,11,1,1]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,2,2]
  , p "xhtml" [3000,2,2,1]
  ]

ghc925_pkgs :: [Cabal.PackageIdentifier]
ghc925_pkgs =
  [ p "Cabal" [3,6,3,0]
  , p "array" [0,5,4,0]
  , p "base" [4,16,4,0]
  , p "binary" [0,8,9,0]
  , p "bytestring" [0,11,3,1]
  , p "containers" [0,6,5,1]
  , p "deepseq" [1,4,6,1]
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4]
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,2]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,15,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,16,0]
  , p "rts" [1,0,2]
  , p "stm" [2,5,0,2]
  , p "template-haskell" [2,18,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [1,2,5,0]
  , p "time" [1,11,1,1]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,2,2]
  , p "xhtml" [3000,2,2,1]
  ]


ghc924_pkgs :: [Cabal.PackageIdentifier]
ghc924_pkgs =
  [ p "Cabal" [3,6,3,0]
  , p "array" [0,5,4,0]
  , p "base" [4,16,3,0]
  , p "binary" [0,8,9,0]
  , p "bytestring" [0,11,3,1]
  , p "containers" [0,6,5,1]
  , p "deepseq" [1,4,6,1]
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4]
  , p "filepath" [1,4,2,2]
  , p "ghc-bignum" [1,2]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,8,0]
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,15,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,13,2]
  , p "rts" [1,0,2]
  , p "stm" [2,5,0,2]
  , p "template-haskell" [2,18,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [1,2,5,0]
  , p "time" [1,11,1,1]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,2,2]
  , p "xhtml" [3000,2,2,1]
  ]

ghc902_pkgs :: [Cabal.PackageIdentifier]
ghc902_pkgs =
  [ p "Cabal" [3,4,1,0]
  , p "array" [0,5,4,0]
  , p "base" [4,15,1,0]
  , p "binary" [0,8,8,0]
  , p "bytestring" [0,10,12,1]
  , p "containers" [0,6,4,1]
  , p "deepseq" [1,4,5,0]
  , p "directory" [1,3,6,2]
  , p "exceptions" [0,10,4]
  , p "filepath" [1,4,2,1]
  , p "ghc-bignum" [1,1]
  , p "ghc-compact" [0,1,0,0]
  , p "ghc-prim" [0,7,0]
  , p "haskeline" [0,8,2]
  , p "hpc" [0,6,1,0]
  , p "integer-gmp" [1,1]
  , p "mtl" [2,2,2]
  , p "parsec" [3,1,14,0]
  , p "pretty" [1,1,3,6]
  , p "process" [1,6,13,2]
  , p "rts" [1,0,2]
  , p "stm" [2,5,0,0]
  , p "template-haskell" [2,17,0,0]
  , p "terminfo" [0,4,1,5]
  , p "text" [1,2,5,0]
  , p "time" [1,9,3]
  , p "transformers" [0,5,6,2]
  , p "unix" [2,7,2,2]
  , p "xhtml" [3000,2,2,1]
  ]

p :: String -> [Int] -> Cabal.PackageIdentifier
p pn vs = Cabal.PackageIdentifier (Cabal.mkPackageName pn) (mkVersion vs)
