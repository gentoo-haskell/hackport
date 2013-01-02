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
                                       , TestSuite(..)
                                       , targetBuildDepends
                                       )
import Data.Maybe ( isJust, isNothing )
import Data.List ( nub )

import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Version as Cabal

import Distribution.Compiler

import qualified Portage.Dependency as Portage
import qualified Portage.Overlay as Portage
import qualified Portage.PackageId as Portage
import qualified Portage.Use as Portage
import qualified Portage.Version as Portage
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

resolveDependencies :: Portage.Overlay -> PackageDescription -> Maybe CompilerId -> EDep
resolveDependencies overlay pkg mcompiler =
    edeps
      {
        dep  = dep2,
        rdep = rdep2
        -- todo: if rdep includes cabal or ghc, make sure it's the same
        -- version as in dep
      }
  where
    dep1  = Portage.simplify_deps ( dep edeps)
    dep2  = Portage.simplifyUseDeps dep1 (dep1++rdep2)
    rdep1  = Portage.simplify_deps (rdep edeps)
    rdep2  = Portage.simplifyUseDeps rdep1 rdep1
    compiler = maybe (fst GHCCore.defaultGHC) id mcompiler

    hasBuildableExes p = any (buildable . buildInfo) . executables $ p
    treatAsLibrary = isJust (Cabal.library pkg)
    haskell_deps
        | treatAsLibrary = map set_build_slot $ map add_profile $ haskellDependencies overlay pkg
        | otherwise      = haskellDependencies overlay pkg
    test_deps
        | (not . null) (testSuites pkg) = testDependencies overlay pkg
        | otherwise = [] -- tests not enabled
    cabal_dep = cabalDependency overlay pkg compiler
    ghc_dep = compilerIdToDependency compiler
    extra_libs = findCLibs pkg
    pkg_config_libs = pkgConfigDependencies overlay pkg
    pkg_config_tools = if null pkg_config_libs
                           then []
                           else [Portage.AnyVersionOf (Portage.mkPackageName "virtual" "pkgconfig") Portage.AnySlot []]
    build_tools = buildToolsDependencies pkg ++ pkg_config_tools
    edeps
        | treatAsLibrary = emptyEDep
                  {
                    dep = cabal_dep
                          : build_tools
                          ++ test_deps,
                    dep_e = [ "${RDEPEND}" ],
                    rdep = set_build_slot ghc_dep
                            : haskell_deps
                            ++ extra_libs
                            ++ pkg_config_libs
                  }
        | otherwise = emptyEDep
                  {
                    dep = ghc_dep
                          : cabal_dep
                          : build_tools
                          ++ haskell_deps
                          ++ test_deps,
                    dep_e = [ "${RDEPEND}" ],
                    rdep = extra_libs ++ pkg_config_libs
                  }
    add_profile    = Portage.addDepUseFlag (Portage.mkQUse "profile")
    set_build_slot = Portage.setSlotDep Portage.AnyBuildTimeSlot

---------------------------------------------------------------
-- Test-suite dependencies
---------------------------------------------------------------

testDependencies :: Portage.Overlay -> PackageDescription -> [Portage.Dependency]
testDependencies overlay pkg@(PackageDescription { package = Cabal.PackageIdentifier { Cabal.pkgName = Cabal.PackageName name}}) =
    [Portage.DependIfUse (Portage.UseFlag "test") (Portage.AllOf $ Portage.simplify_deps deps)]
    where cabalDeps = concat $ map targetBuildDepends $ map testBuildInfo (testSuites pkg)
          cabalDeps' = filter (\(Cabal.Dependency (Cabal.PackageName pname) _) -> pname /= name) cabalDeps
          deps = C2E.convertDependencies overlay (Portage.Category "dev-haskell") cabalDeps'

---------------------------------------------------------------
-- Haskell packages
---------------------------------------------------------------

haskellDependencies :: Portage.Overlay -> PackageDescription -> [Portage.Dependency]
haskellDependencies overlay pkg =
    Portage.simplify_deps
      $ C2E.convertDependencies overlay (Portage.Category "dev-haskell") (buildDepends pkg)

---------------------------------------------------------------
-- Cabal Dependency
---------------------------------------------------------------

-- | Select the most restrictive dependency on Cabal, either the .cabal
-- file's descCabalVersion, or the Cabal GHC shipped with.
cabalDependency :: Portage.Overlay -> PackageDescription -> CompilerId -> Portage.Dependency
cabalDependency overlay pkg (CompilerId GHC ghcVersion@(Cabal.Version versionNumbers _)) =
  head $ C2E.convertDependency overlay
                               (Portage.Category "dev-haskell")
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
  Portage.OrLaterVersionOf (Portage.fromCabalVersion versionNumbers) (Portage.mkPackageName "dev-lang" "ghc") Portage.AnySlot []

---------------------------------------------------------------
-- C Libraries
---------------------------------------------------------------

findCLibs :: PackageDescription -> [Portage.Dependency]
findCLibs (PackageDescription { library = lib, executables = exes }) =
  [ trace ("WARNING: This package depends on a C library we don't know the portage name for: " ++ p ++ ". Check the generated ebuild.")
          (Portage.AnyVersionOf (Portage.mkPackageName "unknown-c-lib" p) Portage.AnySlot [])
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
  m = [ ("z", Portage.AnyVersionOf (Portage.mkPackageName "sys-libs" "zlib") Portage.AnySlot [])
      , ("bz2", Portage.AnyVersionOf (Portage.mkPackageName "app-arch" "bzip2") Portage.AnySlot [])
      , ("mysqlclient", Portage.LaterVersionOf (Portage.Version [4,0] Nothing [] 0) (Portage.mkPackageName "virtual" "mysql") Portage.AnySlot [])
      , ("pq", Portage.LaterVersionOf (Portage.Version [7] Nothing [] 0) (Portage.mkPackageName "dev-db" "postgresql-base") Portage.AnySlot [])
      , ("ev", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "libev") Portage.AnySlot [])
      , ("expat", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "expat") Portage.AnySlot [])
      , ("curl", Portage.AnyVersionOf (Portage.mkPackageName "net-misc" "curl") Portage.AnySlot [])
      , ("xml2", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "libxml2") Portage.AnySlot [])
      , ("mecab", Portage.AnyVersionOf (Portage.mkPackageName "app-text" "mecab") Portage.AnySlot [])
      , ("zmq", Portage.AnyVersionOf (Portage.mkPackageName "net-libs" "zeromq") Portage.AnySlot [])
      , ("SDL", Portage.AnyVersionOf (Portage.mkPackageName "media-libs" "libsdl") Portage.AnySlot [])
      , ("adns", Portage.AnyVersionOf (Portage.mkPackageName "net-libs" "adns") Portage.AnySlot [])
      , ("pcre", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "libpcre") Portage.AnySlot [])
      , ("GL", Portage.AnyVersionOf (Portage.mkPackageName "virtual" "opengl") Portage.AnySlot [])
      , ("GLU", Portage.AnyVersionOf (Portage.mkPackageName "virtual" "glu") Portage.AnySlot [])
      , ("glut", Portage.AnyVersionOf (Portage.mkPackageName "media-libs" "freeglut") Portage.AnySlot [])
      , ("X11", Portage.AnyVersionOf (Portage.mkPackageName "x11-libs" "libX11") Portage.AnySlot [])
      , ("libzip", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "libzip") Portage.AnySlot [])
      , ("ssl", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "openssl") Portage.AnySlot [])
      , ("Judy", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "judy") Portage.AnySlot [])
      , ("fcgi", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "fcgi") Portage.AnySlot [])
      , ("gnutls", Portage.AnyVersionOf (Portage.mkPackageName "net-libs" "gnutls") Portage.AnySlot [])
      , ("idn", Portage.AnyVersionOf (Portage.mkPackageName "net-dns" "libidn") Portage.AnySlot [])
      , ("tre", Portage.AnyVersionOf (Portage.mkPackageName "dev-libs" "tre") Portage.AnySlot [])
      , ("m", Portage.AnyVersionOf (Portage.mkPackageName "virtual" "libc") Portage.AnySlot [])
      , ("asound", Portage.AnyVersionOf (Portage.mkPackageName "media-libs" "alsa-lib") Portage.AnySlot [])
      , ("sqlite3", Portage.OrLaterVersionOf (Portage.Version [3,0] Nothing [] 0) (Portage.mkPackageName "dev-db" "sqlite") Portage.AnySlot [])
      , ("stdc++", Portage.AnyVersionOf (Portage.mkPackageName "sys-devel" "gcc") Portage.AnySlot [Portage.mkUse "cxx"])
      , ("crack", Portage.AnyVersionOf (Portage.mkPackageName "sys-libs" "cracklib") Portage.AnySlot [])
      , ("exif", Portage.AnyVersionOf (Portage.mkPackageName "media-libs" "libexif") Portage.AnySlot [])
      ]

---------------------------------------------------------------
-- Build Tools
---------------------------------------------------------------

buildToolsDependencies :: PackageDescription -> [Portage.Dependency]
buildToolsDependencies (PackageDescription { library = lib, executables = exes }) = nub $
  [ case pkg of
      Just p -> p
      Nothing -> trace ("WARNING: Unknown build tool '" ++ pn ++ "'. Check the generated ebuild.")
                       (Portage.AnyVersionOf (Portage.mkPackageName "unknown-build-tool" pn) Portage.AnySlot [])
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
  [ ("happy", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "happy") Portage.AnySlot [])
  , ("alex", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "alex") Portage.AnySlot [])
  , ("c2hs", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "c2hs") Portage.AnySlot [])
  , ("cabal-install", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "cabal-install") Portage.AnySlot [])
  , ("gtk2hsTypeGen",       Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools") Portage.AnySlot [])
  , ("gtk2hsHookGenerator", Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools") Portage.AnySlot [])
  , ("gtk2hsC2hs",          Portage.AnyVersionOf (Portage.mkPackageName "dev-haskell" "gtk2hs-buildtools") Portage.AnySlot [])
  ]

-- tools that are provided by ghc or some other existing program
-- so we do not need dependencies on them
buildToolsProvided :: [String]
buildToolsProvided = ["hsc2hs"]


---------------------------------------------------------------
-- pkg-config
---------------------------------------------------------------

pkgConfigDependencies :: Portage.Overlay -> PackageDescription -> [Portage.Dependency]
pkgConfigDependencies overlay (PackageDescription { library = lib, executables = exes }) = nub $ resolvePkgConfigs overlay cabalDeps
  where
  cabalDeps = depL ++ depE
  depL = maybe [] (pkgconfigDepends.libBuildInfo) lib
  depE = concatMap pkgconfigDepends (filter buildable (map buildInfo exes))

resolvePkgConfigs :: Portage.Overlay -> [Cabal.Dependency] -> [Portage.Dependency]
resolvePkgConfigs overlay cdeps =
  [ case resolvePkgConfig overlay pkg of
      Just d -> d
      Nothing -> trace ("WARNING: Could not resolve pkg-config: " ++ pn ++ ". Check generated ebuild.")
                       (Portage.AnyVersionOf (Portage.mkPackageName "unknown-pkg-config" pn) Portage.AnySlot [])
  | pkg@(Cabal.Dependency (Cabal.PackageName pn) _range) <- cdeps ]

resolvePkgConfig :: Portage.Overlay -> Cabal.Dependency -> Maybe Portage.Dependency
resolvePkgConfig _overlay (Cabal.Dependency (Cabal.PackageName pn) _cabalVersion) = do
  (cat,portname, slot) <- lookup pn table
  return $ Portage.AnyVersionOf (Portage.mkPackageName cat portname) slot []

table :: [(String, (String, String, Portage.SlotDepend))]
table =
  [
   ("alsa",         ("media-libs", "alsa-lib", Portage.AnySlot))
  ,("gconf-2.0",    ("gnome-base", "gconf", Portage.AnySlot))

  ,("gio-2.0",                ("dev-libs", "glib", Portage.GivenSlot "2"))
  ,("gio-unix-2.0",           ("dev-libs", "glib", Portage.GivenSlot "2"))
  ,("glib-2.0",               ("dev-libs", "glib", Portage.GivenSlot "2"))
  ,("gmodule-2.0",            ("dev-libs", "glib", Portage.GivenSlot "2"))
  ,("gmodule-export-2.0",     ("dev-libs", "glib", Portage.GivenSlot "2"))
  ,("gmodule-no-export-2.0",  ("dev-libs", "glib", Portage.GivenSlot "2"))
  ,("gobject-2.0",            ("dev-libs", "glib", Portage.GivenSlot "2"))
  ,("gthread-2.0",            ("dev-libs", "glib", Portage.GivenSlot "2"))

  ,("gtk+-2.0",            ("x11-libs", "gtk+", Portage.GivenSlot "2"))
  ,("gdk-2.0",             ("x11-libs", "gtk+", Portage.GivenSlot "2"))
  ,("gdk-pixbuf-2.0",      ("x11-libs", "gtk+", Portage.GivenSlot "2"))
  ,("gdk-pixbuf-xlib-2.0", ("x11-libs", "gtk+", Portage.GivenSlot "2"))
  ,("gdk-x11-2.0",         ("x11-libs", "gtk+", Portage.GivenSlot "2"))
  ,("gtk+-unix-print-2.0", ("x11-libs", "gtk+", Portage.GivenSlot "2"))
  ,("gtk+-x11-2.0",        ("x11-libs", "gtk+", Portage.GivenSlot "2"))

  ,("cairo",            ("x11-libs", "cairo", Portage.AnySlot)) -- need [svg] for dev-haskell/cairo
  ,("cairo-ft",         ("x11-libs", "cairo", Portage.AnySlot))
  ,("cairo-ps",         ("x11-libs", "cairo", Portage.AnySlot))
  ,("cairo-png",        ("x11-libs", "cairo", Portage.AnySlot))
  ,("cairo-pdf",        ("x11-libs", "cairo", Portage.AnySlot))
  ,("cairo-svg",        ("x11-libs", "cairo", Portage.AnySlot))
  ,("cairo-xlib",         ("x11-libs", "cairo", Portage.AnySlot))
  ,("cairo-xlib-xrender", ("x11-libs", "cairo", Portage.AnySlot))

  ,("pangocairo",       ("x11-libs", "pango", Portage.AnySlot))
  ,("pangoft2",         ("x11-libs", "pango", Portage.AnySlot))
  ,("pango",            ("x11-libs", "pango", Portage.AnySlot))
  ,("pangoxft",         ("x11-libs", "pango", Portage.AnySlot))
  ,("pangox",           ("x11-libs", "pango", Portage.AnySlot))

  ,("libglade-2.0", ("gnome-base", "libglade", Portage.AnySlot))
  ,("gnome-vfs-2.0", ("gnome-base", "gnome-vfs", Portage.AnySlot))
  ,("gnome-vfs-module-2.0", ("gnome-base", "gnome-vfs", Portage.AnySlot))
  ,("webkit-1.0", ("net-libs","webkit-gtk", Portage.GivenSlot "2"))

  ,("gstreamer-0.10",              ("media-libs", "gstreamer", Portage.AnySlot))
  ,("gstreamer-base-0.10",         ("media-libs", "gstreamer", Portage.AnySlot))
  ,("gstreamer-check-0.10",        ("media-libs", "gstreamer", Portage.AnySlot))
  ,("gstreamer-controller-0.10",   ("media-libs", "gstreamer", Portage.AnySlot))
  ,("gstreamer-dataprotocol-0.10", ("media-libs", "gstreamer", Portage.AnySlot))
  ,("gstreamer-net-0.10",          ("media-libs", "gstreamer", Portage.AnySlot))

  ,("gstreamer-app-0.10",          ("media-libs", "gst-plugins-base", Portage.AnySlot))
  ,("gstreamer-audio-0.10",        ("media-libs", "gst-plugins-base", Portage.AnySlot))
  ,("gstreamer-video-0.10",        ("media-libs", "gst-plugins-base", Portage.AnySlot))
  ,("gstreamer-plugins-base-0.10", ("media-libs", "gst-plugins-base", Portage.AnySlot))

  ,("gtksourceview-2.0",           ("x11-libs", "gtksourceview", Portage.GivenSlot "2.0"))
  ,("librsvg-2.0",                 ("gnome-base","librsvg", Portage.AnySlot))
  ,("vte",                         ("x11-libs","vte", Portage.GivenSlot "0"))
  ,("gtkglext-1.0",                ("x11-libs","gtkglext", Portage.AnySlot))

  ,("curl",                        ("net-misc", "curl", Portage.AnySlot))
  ,("libxml2",                     ("dev-libs", "libxml2", Portage.AnySlot))
  ,("libgsasl",                    ("virtual", "gsasl", Portage.AnySlot))
  ,("libzip",                      ("dev-libs", "libzip", Portage.AnySlot))
  ,("gnutls",                      ("net-libs", "gnutls", Portage.AnySlot))
  ,("libidn",                      ("net-dns", "libidn", Portage.AnySlot))
  ,("libxml-2.0",                  ("dev-libs", "libxml2", Portage.AnySlot))
  ]
