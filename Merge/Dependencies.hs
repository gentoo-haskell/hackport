{- | Merge a package from hackage to an ebuild.  

Merging a library
=================

Compile time:
  ghc
  cabal
  build tools
  deps (haskell dependencies)
  extra-libs (c-libs)
  pkg-config (c-libs)

Run time:
  ghc
  deps (haskell dependencies)
  extra-libs (c-libs)
  pkg-config (c-libs)

RDEPEND="ghc ${DEPS} ${EXTRALIBS}"
DEPEND="${RDEPEND} cabal ${BUILDTOOLS}"

Merging an executable
=====================
Packages with both executable and library must be treated as libraries, as it will impose a stricter DEPEND.

Compile time:
  ghc
  cabal
  build tools
  deps (haskell dependencies)
  extra-libs (c-libs)
  pkg-config (c-libs)

Run time:
  extra-libs (c-libs)
  pkg-config (c-libs)

RDEPEND="${EXTRALIBS}"
DEPEND="${RDEPEND} ghc cabal ${DEPS} ${BUILDTOOLS}"

-}

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
                                       , hasLibs
                                       , specVersion
                                       )
import Data.Maybe ( isNothing )
import Data.List ( nub )

import qualified Distribution.Package as Cabal
import qualified Distribution.Version as Cabal
import Distribution.Compiler

import qualified Portage.Version as Portage
import qualified Portage.PackageId as Portage
import qualified Portage.Dependency as Portage
import qualified Cabal2Ebuild as C2E

import qualified Portage.GHCCore as GHCCore

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

resolveDependencies :: PackageDescription -> Maybe CompilerId -> EDep
resolveDependencies pkg mcompiler =
    edeps
      {
        dep  = Portage.simplify_deps ( dep edeps),
        rdep = Portage.simplify_deps (rdep edeps)
        -- todo: if rdep includes cabal or ghc, make sure it's the same
        -- version as in dep
      }
  where
    compiler = maybe (fst GHCCore.defaultGHC) id mcompiler

    hasBuildableExes p = any (buildable . buildInfo) . executables $ p
    treatAsLibrary = (not . hasBuildableExes) pkg || hasLibs pkg
    haskell_deps = haskellDependencies pkg
    cabal_dep = cabalDependency pkg compiler
    ghc_dep = compilerIdToDependency compiler
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

-- | Select the most restrictive dependency on Cabal, either the .cabal
-- file's descCabalVersion, or the Cabal GHC shipped with.
cabalDependency :: PackageDescription -> CompilerId -> Portage.Dependency
cabalDependency pkg (CompilerId GHC ghcVersion@(Cabal.Version versionNumbers _)) =
  head $ C2E.convertDependency (Portage.Category "dev-haskell")
                               (Cabal.Dependency (Cabal.PackageName "Cabal")
                                                 finalCabalDep)
  where
    userCabalVersion = Cabal.orLaterVersion (specVersion pkg)
    shippedCabalVersion = GHCCore.cabalFromGHC versionNumbers
    shippedCabalDep = maybe Cabal.anyVersion
                            (\shipped -> Cabal.intersectVersionRanges
                                (Cabal.thisVersion  shipped)
                                (Cabal.laterVersion shipped))
                            shippedCabalVersion
    finalCabalDep = Cabal.simplifyVersionRange
                                (Cabal.intersectVersionRanges
                                          userCabalVersion
                                          shippedCabalDep)

---------------------------------------------------------------
-- GHC Dependency
---------------------------------------------------------------

compilerIdToDependency :: CompilerId -> Portage.Dependency
compilerIdToDependency (CompilerId GHC versionNumbers) =
  Portage.OrLaterVersionOf (Portage.fromCabalVersion versionNumbers) (Portage.mkPackageName "dev-lang" "ghc")

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
      , ("expat", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "expat"))
      , ("curl", Portage.AnyVersionOf (Portage.mkPackageName "net-misc" "curl"))
      , ("xml2", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "libxml2"))
      , ("mecab", Portage.AnyVersionOf (Portage.mkPackageName "app-text" "mecab"))
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
  cabalDeps = filter notProvided $ depL ++ depE
  depL = maybe [] (buildTools.libBuildInfo) lib
  depE = concatMap buildTools (filter buildable (map buildInfo exes))
  notProvided (Cabal.Dependency (Cabal.PackageName pn) _range) = pn `notElem` buildToolsProvided

buildToolsTable :: [(String, Portage.Dependency)]
buildToolsTable =
  [ ("happy", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "happy"))
  , ("alex", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "alex"))
  , ("c2hs", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "c2hs"))
  , ("gtk2hsTypeGen",       Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools"))
  , ("gtk2hsHookGenerator", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools"))
  , ("gtk2hsC2hs",          Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools"))
  ]

-- tools that are provided by ghc or some other existing program
-- so we do not need dependencies on them
buildToolsProvided :: [String]
buildToolsProvided = ["hsc2hs"]


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

  ,("gio-2.0",                ("dev-libs", "glib"))
  ,("gio-unix-2.0",           ("dev-libs", "glib"))
  ,("glib-2.0",               ("dev-libs", "glib"))
  ,("gmodule-2.0",            ("dev-libs", "glib"))
  ,("gmodule-export-2.0",     ("dev-libs", "glib"))
  ,("gmodule-no-export-2.0",  ("dev-libs", "glib"))
  ,("gobject-2.0",            ("dev-libs", "glib"))
  ,("gthread-2.0",            ("dev-libs", "glib")) -- should be slot 2

  ,("gtk+-2.0",            ("x11-libs", "gtk+")) -- should be slot 2
  ,("gdk-2.0",             ("x11-libs", "gtk+"))
  ,("gdk-pixbuf-2.0",      ("x11-libs", "gtk+"))
  ,("gdk-pixbuf-xlib-2.0", ("x11-libs", "gtk+"))
  ,("gdk-x11-2.0",         ("x11-libs", "gtk+"))
  ,("gtk+-unix-print-2.0", ("x11-libs", "gtk+"))
  ,("gtk+-x11-2.0",        ("x11-libs", "gtk+"))

  ,("cairo",            ("x11-libs", "cairo")) -- need [svg] for dev-haskell/cairo
  ,("cairo-ft",         ("x11-libs", "cairo"))
  ,("cairo-ps",         ("x11-libs", "cairo"))
  ,("cairo-png",        ("x11-libs", "cairo"))
  ,("cairo-pdf",        ("x11-libs", "cairo"))
  ,("cairo-svg",        ("x11-libs", "cairo"))
  ,("cairo-xlib",         ("x11-libs", "cairo"))
  ,("cairo-xlib-xrender", ("x11-libs", "cairo"))

  ,("pangocairo",       ("x11-libs", "pango"))
  ,("pangoft2",         ("x11-libs", "pango"))
  ,("pango",            ("x11-libs", "pango"))
  ,("pangoxft",         ("x11-libs", "pango"))
  ,("pangox",           ("x11-libs", "pango"))

  ,("libglade-2.0", ("gnome-base", "libglade"))
  ,("gnome-vfs-2.0", ("gnome-base", "gnome-vfs"))
  ,("gnome-vfs-module-2.0", ("gnome-base", "gnome-vfs"))
  ,("webkit-1.0", ("net-libs","webkit-gtk"))

  ,("gstreamer-0.10",              ("media-libs", "gstreamer"))
  ,("gstreamer-base-0.10",         ("media-libs", "gstreamer"))
  ,("gstreamer-check-0.10",        ("media-libs", "gstreamer"))
  ,("gstreamer-controller-0.10",   ("media-libs", "gstreamer"))
  ,("gstreamer-dataprotocol-0.10", ("media-libs", "gstreamer"))
  ,("gstreamer-net-0.10",          ("media-libs", "gstreamer"))

  ,("gstreamer-app-0.10",          ("media-libs", "gst-plugins-base"))
  ,("gstreamer-audio-0.10",        ("media-libs", "gst-plugins-base"))
  ,("gstreamer-video-0.10",        ("media-libs", "gst-plugins-base"))
  ,("gstreamer-plugins-base-0.10", ("media-libs", "gst-plugins-base"))

  ,("gtksourceview-2.0",           ("x11-libs", "gtksourceview"))
  ,("librsvg-2.0",                 ("gnome-base","librsvg"))
  ,("vte",                         ("x11-libs","vte"))
  ,("gtkglext-1.0",                ("x11-libs","gtkglext"))

  ,("curl",                        ("net-misc", "curl"))
  ,("libxml2",                     ("dev-libs", "libxml2"))

  ]
