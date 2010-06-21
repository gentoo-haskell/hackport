module Merge.Dependencies
  ( EDep(..)
  , resolveDependencies
  ) where

import Distribution.PackageDescription ( PackageDescription(..)
                                       , libBuildInfo
                                       , buildInfo
                                       , buildable
                                       , extraLibs
                                       , buildTools
                                       , pkgconfigDepends
                                       , hasLibs )
import Data.Maybe ( isNothing )
import Data.List ( nub )

import qualified Distribution.Package as Cabal

import qualified Portage.PackageId as Portage
import qualified Portage.Dependency as Portage
import qualified Cabal2Ebuild as C2E

import Debug.Trace ( trace )

-- | Dependencies of an ebuild
data EDep = EDep
  {
    rdep :: [Portage.Dependency],
    rdep_e :: [String],
    dep :: [Portage.Dependency],
    dep_e :: [String]
  }

emptyEDep :: EDep
emptyEDep = EDep
  {
    rdep = [],
    rdep_e = [],
    dep = [],
    dep_e = []
  }

resolveDependencies :: PackageDescription -> EDep
resolveDependencies pkg =
    edeps
      {
        dep  = Portage.simplify_deps ( dep edeps),
        rdep = Portage.simplify_deps (rdep edeps)
      }
  where
    hasBuildableExes p = any (buildable . buildInfo) . executables $ p
    treatAsLibrary = (not . hasBuildableExes) pkg || hasLibs pkg
    haskell_deps = haskellDependencies pkg
    cabal_dep = cabalDependency pkg
    ghc_dep = ghcDependency pkg
    extra_libs = findCLibs pkg
    build_tools = buildToolsDependencies pkg
    pkg_config = pkgConfigDependencies pkg
    edeps
        | treatAsLibrary = emptyEDep
                  {
                    dep = cabal_dep
                          : build_tools,
                    dep_e = [ "${RDEPEND}" ],
                    rdep = ghc_dep
                            : haskell_deps
                            ++ extra_libs
                            ++ pkg_config
                  }
        | otherwise = emptyEDep
                  {
                    dep = ghc_dep
                          : cabal_dep
                          : build_tools
                          ++ haskell_deps,
                    dep_e = [ "${RDEPEND}" ],
                    rdep = extra_libs ++ pkg_config
                  }

---------------------------------------------------------------
-- Haskell packages
---------------------------------------------------------------

haskellDependencies :: PackageDescription -> [Portage.Dependency]
haskellDependencies pkg =
  Portage.simplify_deps
    $ C2E.convertDependencies (Portage.Category "dev-haskell") (buildDepends pkg)

---------------------------------------------------------------
-- Cabal Dependency
---------------------------------------------------------------

cabalDependency :: PackageDescription -> Portage.Dependency
cabalDependency pkg =
  head $ C2E.convertDependency (Portage.Category "dev-haskell")
                               (Cabal.Dependency (Cabal.PackageName "Cabal")
                                                 (descCabalVersion pkg))

---------------------------------------------------------------
-- GHC Dependency
---------------------------------------------------------------

ghcDependency :: PackageDescription -> Portage.Dependency
ghcDependency _pkg = C2E.default_ghc_dependency
 
---------------------------------------------------------------
-- C Libraries
---------------------------------------------------------------

findCLibs :: PackageDescription -> [Portage.Dependency]
findCLibs (PackageDescription { library = lib, executables = exes }) =
  [ trace ("WARNING: This package depends on a C library we don't know the portage name for: " ++ p ++ ". Check the generated ebuild.")
          (Portage.AnyVersionOf (Portage.mkPackageName "unknown-c-lib" p))
  | p <- notFound
  ] ++ 
  found
  where
  libE = maybe [] (extraLibs.libBuildInfo) lib
  exeE = concatMap extraLibs (filter buildable (map buildInfo exes))
  allE = libE ++ exeE

  notFound = [ p | p <- allE, isNothing (staticTranslateExtraLib p) ]
  found =    [ p | Just p <- map staticTranslateExtraLib allE ]
  

staticTranslateExtraLib :: String -> Maybe Portage.Dependency
staticTranslateExtraLib lib = lookup lib m
  where
  m = [ ("z", Portage.AnyVersionOf (Portage.mkPackageName "sys-libs" "zlib"))
      , ("bz2", Portage.AnyVersionOf (Portage.mkPackageName "sys-libs" "bzlib"))
      , ("mysqlclient", Portage.LaterVersionOf (Portage.Version [4,0] Nothing [] 0) (Portage.mkPackageName "virtual" "mysql"))
      , ("pq", Portage.LaterVersionOf (Portage.Version [7] Nothing [] 0) (Portage.mkPackageName "virtual" "postgresql-base"))
      , ("ev", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "libev"))
      ]

---------------------------------------------------------------
-- Build Tools
---------------------------------------------------------------

buildToolsDependencies :: PackageDescription -> [Portage.Dependency]
buildToolsDependencies (PackageDescription { library = lib, executables = exes }) = nub $
  [ case pkg of
      Just p -> p
      Nothing -> trace ("WARNING: Unknown build tool '" ++ pn ++ "'. Check the generated ebuild.")
                       (Portage.AnyVersionOf (Portage.mkPackageName "unknown-build-tool" pn))
  | Cabal.Dependency (Cabal.PackageName pn) _range <- cabalDeps
  , pkg <- return (lookup pn buildToolsTable) 
  ]
  where
  cabalDeps = depL ++ depE
  depL = maybe [] (buildTools.libBuildInfo) lib
  depE = concatMap buildTools (filter buildable (map buildInfo exes))

buildToolsTable :: [(String, Portage.Dependency)]
buildToolsTable =
  [ ("happy", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "happy"))
  , ("alex", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "alex"))
  , ("c2hs", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "c2hs"))
  , ("gtk2hsTypeGen",       Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools"))
  , ("gtk2hsHookGenerator", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools"))
  , ("gtk2hsC2hs",          Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools"))
  ]

---------------------------------------------------------------
-- pkg-config
---------------------------------------------------------------

pkgConfigDependencies :: PackageDescription -> [Portage.Dependency]
pkgConfigDependencies (PackageDescription { library = lib, executables = exes }) = nub $ resolvePkgConfigs cabalDeps
  where
  cabalDeps = depL ++ depE
  depL = maybe [] (pkgconfigDepends.libBuildInfo) lib
  depE = concatMap pkgconfigDepends (filter buildable (map buildInfo exes))

resolvePkgConfigs :: [Cabal.Dependency] -> [Portage.Dependency]
resolvePkgConfigs cdeps =
  [ case resolvePkgConfig pkg of
      Just d -> d
      Nothing -> trace ("WARNING: Could not resolve pkg-config: " ++ pn ++ ". Check generated ebuild.")
                       (Portage.AnyVersionOf (Portage.mkPackageName "unknown-pkg-config" pn))
  | pkg@(Cabal.Dependency (Cabal.PackageName pn) _range) <- cdeps ]

resolvePkgConfig :: Cabal.Dependency -> Maybe Portage.Dependency
resolvePkgConfig (Cabal.Dependency (Cabal.PackageName pn) _cabalVersion) = do
  (cat,portname) <- lookup pn table
  return . head $ (C2E.convertDependency (Portage.Category cat) (Cabal.Dependency (Cabal.PackageName portname) _cabalVersion))

table :: [(String, (String, String))]
table =
  [("gconf-2.0",    ("gnome-base", "gconf"))
  ,("gthread-2.0",  ("dev-libs", "glib")) -- should be slot 2
  ,("gtk+-2.0",     ("x11-libs", "gtk+")) -- should be slot 2
  ,("cairo",        ("x11-libs", "cairo"))
  ,("cairo-ps",        ("x11-libs", "cairo"))
  ,("cairo-pdf",        ("x11-libs", "cairo"))
  ,("cairo-svg",        ("x11-libs", "cairo")) -- need [svg] for dev-haskell/cairo
  ,("pango",        ("x11-libs", "pango"))
  ,("gio-2.0",      ("dev-libs", "glib"))
  ]
