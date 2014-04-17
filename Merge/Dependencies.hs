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
  , intersection
  , difference
  , null
  ) where

import Distribution.PackageDescription ( PackageDescription(..)
                                       , libBuildInfo
                                       , buildInfo
                                       , buildable
                                       , extraLibs
                                       , buildTools
                                       , pkgconfigDepends
                                       , specVersion
                                       , TestSuite(..)
                                       , targetBuildDepends
                                       )
import Prelude hiding (null)
import Data.Maybe ( isJust, isNothing )
import Data.Monoid ( Monoid, mempty, mappend)
import Data.List ( nub )
import qualified Data.List as L
import qualified Data.Set as S

import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Version as Cabal

import Distribution.Compiler

import qualified Portage.Cabal as Portage
import qualified Portage.Dependency as Portage
import qualified Portage.Overlay as Portage
import qualified Portage.PackageId as Portage
import qualified Portage.Use as Portage
import qualified Portage.Tables as Portage
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
  deriving (Show, Eq)

instance Monoid EDep where
  mempty = EDep
      {
        rdep = [],
        rdep_e = [],
        dep = [],
        dep_e = []
      }
  (EDep rdepA rdep_eA depA dep_eA) `mappend` (EDep rdepB rdep_eB depB dep_eB) = EDep
    { rdep = rdepA ++ rdepB
    , rdep_e = S.toList $ (S.fromList rdep_eA) `S.union` (S.fromList rdep_eB)
    , dep  = depA ++ depB
    , dep_e = S.toList $ (S.fromList dep_eA) `S.union` (S.fromList dep_eB)
    }


intersection :: EDep -> EDep -> EDep
intersection (EDep a1 a2 a3 a4) (EDep b1 b2 b3 b4) = EDep (L.intersect a1 b1)
                                                          (L.intersect a2 b2)
                                                          (L.intersect a3 b3)
                                                          (L.intersect a4 b4)

difference :: EDep -> EDep -> EDep
difference (EDep a1 a2 a3 a4) (EDep b1 b2 b3 b4) = EDep (f a1 b1)
                                                        (f a2 b2)
                                                        (f a3 b3)
                                                        (f a4 b4)
  where f a b = L.filter (`L.notElem` b) a

null :: EDep -> Bool
null e = e == mempty

resolveDependencies :: Portage.Overlay -> PackageDescription -> CompilerId
                    -> [Cabal.PackageName] -> Cabal.PackageName
                    -> EDep
resolveDependencies overlay pkg compiler ghc_package_names merged_cabal_pkg_name =
    edeps
      {
        dep  = dep2,
        rdep = rdep2
        -- todo: if rdep includes cabal or ghc, make sure it's the same
        -- version as in dep
      }
  where
    dep1  = dep edeps
    dep2  = Portage.simplifyUseDeps dep1 (dep1 ++ rdep2)
    rdep1  = rdep edeps
    rdep2  = Portage.simplifyUseDeps rdep1 rdep1

    -- hasBuildableExes p = any (buildable . buildInfo) . executables $ p
    treatAsLibrary :: Bool
    treatAsLibrary = isJust (Cabal.library pkg)
    haskell_deps :: [Portage.Dependency]
    haskell_deps
        | treatAsLibrary = map Portage.set_build_slot $ map add_profile $ haskellDependencies overlay (buildDepends pkg)
        | otherwise      = haskellDependencies overlay (buildDepends pkg)
    test_deps :: [Portage.Dependency]
    test_deps
        | (not . L.null) (testSuites pkg) = testDependencies overlay pkg ghc_package_names merged_cabal_pkg_name
        | otherwise = [] -- tests not enabled
    cabal_dep :: Portage.Dependency
    cabal_dep = cabalDependency overlay pkg compiler
    ghc_dep :: Portage.Dependency
    ghc_dep = compilerIdToDependency compiler
    extra_libs :: [Portage.Dependency]
    extra_libs = findCLibs pkg
    pkg_config_libs :: [Portage.Dependency]
    pkg_config_libs = pkgConfigDependencies overlay pkg
    pkg_config_tools :: [Portage.Dependency]
    pkg_config_tools = if L.null pkg_config_libs
                           then []
                           else [any_c_p "virtual" "pkgconfig"]
    build_tools :: [Portage.Dependency]
    build_tools = buildToolsDependencies pkg ++ pkg_config_tools
    edeps :: EDep
    edeps
        | treatAsLibrary = mempty
                  {
                    dep = cabal_dep
                          : build_tools
                          ++ test_deps,
                    dep_e = [ "${RDEPEND}" ],
                    rdep = Portage.set_build_slot ghc_dep
                            : haskell_deps
                            ++ extra_libs
                            ++ pkg_config_libs
                  }
        | otherwise = mempty
                  {
                    dep = ghc_dep
                          : cabal_dep
                          : build_tools
                          ++ haskell_deps
                          ++ test_deps,
                    dep_e = [ "${RDEPEND}" ],
                    rdep = extra_libs ++ pkg_config_libs
                  }
    add_profile    = Portage.addDepUseFlag (Portage.mkQUse (Portage.Use "profile"))

---------------------------------------------------------------
-- Test-suite dependencies
---------------------------------------------------------------

testDependencies :: Portage.Overlay -> PackageDescription -> [Cabal.PackageName] -> Cabal.PackageName -> [Portage.Dependency]
testDependencies overlay pkg ghc_package_names merged_cabal_pkg_name =
    [Portage.mkUseDependency (True, Portage.Use "test") (Portage.DependAllOf deps)]
    where cabalDeps = concat $ map targetBuildDepends $ map testBuildInfo (testSuites pkg)
          cabalDeps' = fst $ Portage.partition_depends ghc_package_names merged_cabal_pkg_name cabalDeps
          deps = C2E.convertDependencies overlay (Portage.Category "dev-haskell") cabalDeps'

---------------------------------------------------------------
-- Haskell packages
---------------------------------------------------------------

haskellDependencies :: Portage.Overlay -> [Cabal.Dependency] {- PackageDescription -} -> [Portage.Dependency]
haskellDependencies overlay deps =
    C2E.convertDependencies overlay (Portage.Category "dev-haskell") deps

---------------------------------------------------------------
-- Cabal Dependency
---------------------------------------------------------------

-- | Select the most restrictive dependency on Cabal, either the .cabal
-- file's descCabalVersion, or the Cabal GHC shipped with.
cabalDependency :: Portage.Overlay -> PackageDescription -> CompilerId -> Portage.Dependency
cabalDependency overlay pkg ~(CompilerId GHC _ghcVersion@(Cabal.Version versionNumbers _)) =
         C2E.convertDependency overlay
                               (Portage.Category "dev-haskell")
                               (Cabal.Dependency (Cabal.PackageName "Cabal")
                                                 finalCabalDep)
  where
    userCabalVersion = Cabal.orLaterVersion (specVersion pkg)
    shippedCabalVersion = GHCCore.cabalFromGHC versionNumbers
    shippedCabalDep = maybe Cabal.anyVersion Cabal.orLaterVersion shippedCabalVersion
    finalCabalDep = Cabal.simplifyVersionRange
                                (Cabal.intersectVersionRanges
                                          userCabalVersion
                                          shippedCabalDep)

---------------------------------------------------------------
-- GHC Dependency
---------------------------------------------------------------

compilerIdToDependency :: CompilerId -> Portage.Dependency
compilerIdToDependency ~(CompilerId GHC versionNumbers) =
  at_least_c_p_v "dev-lang" "ghc" (Cabal.versionBranch versionNumbers)

---------------------------------------------------------------
-- C Libraries
---------------------------------------------------------------

findCLibs :: PackageDescription -> [Portage.Dependency]
findCLibs (PackageDescription { library = lib, executables = exes }) =
  [ trace ("WARNING: This package depends on a C library we don't know the portage name for: " ++ p ++ ". Check the generated ebuild.")
          (any_c_p "unknown-c-lib" p)
  | p <- notFound
  ] ++
  found
  where
  libE = maybe [] (extraLibs.libBuildInfo) lib
  exeE = concatMap extraLibs (filter buildable (map buildInfo exes))
  allE = libE ++ exeE

  notFound = [ p | p <- allE, isNothing (staticTranslateExtraLib p) ]
  found =    [ p | Just p <- map staticTranslateExtraLib allE ]

any_c_p_s_u :: String -> String -> Portage.SlotDepend -> [Portage.UseFlag] -> Portage.Dependency
any_c_p_s_u cat pn slot uses = Portage.DependAtom $
    Portage.Atom (Portage.mkPackageName cat pn)
                 (Portage.DRange Portage.ZeroB Portage.InfinityB)
                 (Portage.DAttr slot uses)

any_c_p :: String -> String -> Portage.Dependency
any_c_p cat pn = any_c_p_s_u cat pn Portage.AnySlot []

at_least_c_p_v :: String -> String -> [Int] -> Portage.Dependency
at_least_c_p_v cat pn v = Portage.DependAtom $
  Portage.Atom (Portage.mkPackageName cat pn)
               (Portage.DRange (Portage.NonstrictLB (Portage.Version v Nothing [] 0)) Portage.InfinityB)
               (Portage.DAttr Portage.AnySlot [])

staticTranslateExtraLib :: String -> Maybe Portage.Dependency
staticTranslateExtraLib lib = lookup lib m
  where
  m = [ ("z", any_c_p "sys-libs" "zlib")
      , ("bz2", any_c_p "app-arch" "bzip2")
      , ("mysqlclient", at_least_c_p_v "virtual" "mysql" [4,0])
      , ("pq", at_least_c_p_v "dev-db" "postgresql-base" [7])
      , ("ev", any_c_p "dev-libs" "libev")
      , ("expat", any_c_p "dev-libs" "expat")
      , ("curl", any_c_p "net-misc" "curl")
      , ("xml2", any_c_p "dev-libs" "libxml2")
      , ("mecab", any_c_p "app-text" "mecab")
      , ("zmq", any_c_p "net-libs" "zeromq")
      , ("SDL", any_c_p "media-libs" "libsdl")
      , ("adns", any_c_p "net-libs" "adns")
      , ("pcre", any_c_p "dev-libs" "libpcre")
      , ("GL", any_c_p "virtual" "opengl")
      , ("GLU", any_c_p "virtual" "glu")
      , ("glut", any_c_p "media-libs" "freeglut")
      , ("X11", any_c_p "x11-libs" "libX11")
      , ("libzip", any_c_p "dev-libs" "libzip")
      , ("ssl", any_c_p "dev-libs" "openssl")
      , ("Judy", any_c_p "dev-libs" "judy")
      , ("fcgi", any_c_p "dev-libs" "fcgi")
      , ("gnutls", any_c_p "net-libs" "gnutls")
      , ("idn", any_c_p "net-dns" "libidn")
      , ("tre", any_c_p "dev-libs" "tre")
      , ("m", any_c_p "virtual" "libc")
      , ("asound", any_c_p "media-libs" "alsa-lib")
      , ("sqlite3", at_least_c_p_v "dev-db" "sqlite" [3,0])
      , ("stdc++", any_c_p_s_u "sys-devel" "gcc" Portage.AnySlot [Portage.mkUse (Portage.Use "cxx")])
      , ("crack", any_c_p "sys-libs" "cracklib")
      , ("exif", any_c_p "media-libs" "libexif")
      , ("IL", any_c_p "media-libs" "devil")
      , ("Imlib2", any_c_p "media-libs" "imlib2")
      , ("pcap", any_c_p "net-libs" "libpcap")
      , ("lber", any_c_p "net-nds" "openldap")
      , ("ldap", any_c_p "net-nds" "openldap")
      , ("expect", any_c_p "dev-tcltk" "expect")
      , ("tcl", any_c_p "dev-lang" "tcl")
      , ("Xext", any_c_p "x11-libs" "libXext")
      , ("Xrandr", any_c_p "x11-libs" "libXrandr")
      , ("crypto", any_c_p "dev-libs" "openssl")
      , ("gmp", any_c_p "dev-libs" "gmp")
      , ("fuse", any_c_p "sys-fs" "fuse")
      , ("zip", any_c_p "dev-libs" "libzip")
      , ("QtCore", any_c_p "dev-qt" "qtcore")
      , ("QtDeclarative", any_c_p "dev-qt" "qtdeclarative")
      , ("QtGui", any_c_p "dev-qt" "qtgui")
      , ("QtOpenGL", any_c_p "dev-qt" "qtopengl")
      , ("QtScript", any_c_p "dev-qt" "qtscript")
      , ("gsl", any_c_p "sci-libs" "gsl")
      , ("gslcblas", any_c_p "sci-libs" "gsl")
      , ("mkl_core", any_c_p "sci-libs" "mkl")
      , ("mkl_intel_lp64", any_c_p "sci-libs" "mkl")
      , ("mkl_lapack", any_c_p "sci-libs" "mkl")
      , ("mkl_sequential", any_c_p "sci-libs" "mkl")
      , ("Xi", any_c_p "x11-libs" "libXi")
      , ("Xxf86vm", any_c_p "x11-libs" "libXxf86vm")
      , ("pthread", any_c_p "virtual" "libc")
      , ("panelw", any_c_p "sys-libs" "ncurses")
      , ("ncursesw", any_c_p "sys-libs" "ncurses")
      , ("ftgl", any_c_p "media-libs" "ftgl")
      , ("glpk", any_c_p "sci-mathematics" "glpk")
      , ("sndfile", any_c_p "media-libs" "libsndfile")
      , ("portaudio", any_c_p "media-libs" "portaudio")
      , ("icudata", any_c_p "dev-libs" "icu")
      , ("icui18n", any_c_p "dev-libs" "icu")
      , ("icuuc", any_c_p "dev-libs" "icu")
      , ("chipmunk", any_c_p "sci-physics" "chipmunk")
      , ("alut", any_c_p "media-libs" "freealut")
      , ("openal", any_c_p "media-libs" "openal")
      , ("iw", any_c_p "net-wireless" "wireless-tools")
      , ("attr", any_c_p "sys-apps" "attr")
      , ("ncurses", any_c_p "sys-libs" "ncurses")
      , ("panel", any_c_p "sys-libs" "ncurses")
      ]

---------------------------------------------------------------
-- Build Tools
---------------------------------------------------------------

buildToolsDependencies :: PackageDescription -> [Portage.Dependency]
buildToolsDependencies (PackageDescription { library = lib, executables = exes }) = nub $
  [ case pkg of
      Just p -> p
      Nothing -> trace ("WARNING: Unknown build tool '" ++ pn ++ "'. Check the generated ebuild.")
                       (any_c_p "unknown-build-tool" pn)
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
  [ ("happy", any_c_p "dev-haskell" "happy")
  , ("alex", any_c_p "dev-haskell" "alex")
  , ("c2hs", any_c_p "dev-haskell" "c2hs")
  , ("cabal-install", any_c_p "dev-haskell" "cabal-install")
  , ("gtk2hsTypeGen",       any_c_p "dev-haskell" "gtk2hs-buildtools")
  , ("gtk2hsHookGenerator", any_c_p "dev-haskell" "gtk2hs-buildtools")
  , ("gtk2hsC2hs",          any_c_p "dev-haskell" "gtk2hs-buildtools")
  , ("cabal",               any_c_p "dev-haskell" "cabal-install")
  , ("llvm-config",         any_c_p "sys-devel" "llvm")
  , ("cpphs",               any_c_p "dev-haskell" "cpphs")
  , ("ghc",                 any_c_p "dev-lang" "ghc")
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
                       (any_c_p "unknown-pkg-config" pn)
  | pkg@(Cabal.Dependency (Cabal.PackageName pn) _range) <- cdeps ]

resolvePkgConfig :: Portage.Overlay -> Cabal.Dependency -> Maybe Portage.Dependency
resolvePkgConfig _overlay (Cabal.Dependency (Cabal.PackageName pn) _cabalVersion) = do
  (cat,portname, slot) <- lookup pn pkgconfig_table
  return $ any_c_p_s_u cat portname slot []

pkgconfig_table :: [(String, (String, String, Portage.SlotDepend))]
pkgconfig_table =
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
  ,("yaml-0.1",                    ("dev-libs", "libyaml", Portage.AnySlot))
  ,("QtCore",                      ("dev-qt", "qtcore", Portage.AnySlot))
  ,("lua",                         ("dev-lang", "lua", Portage.AnySlot))
  ,("QtDeclarative",               ("dev-qt", "qtdeclarative", Portage.AnySlot))
  ,("QtGui",                       ("dev-qt", "qtgui", Portage.AnySlot))
  ,("QtOpenGL",                    ("dev-qt", "qtopengl", Portage.AnySlot))
  ,("QtScript",                    ("dev-qt", "qtscript", Portage.AnySlot))
  ,("ImageMagick",                 ("media-gfx", "imagemagick", Portage.AnySlot))
  ,("MagickWand",                  ("media-gfx", "imagemagick", Portage.AnySlot))
  ,("ncurses",                     ("sys-libs", "ncurses", Portage.AnySlot))
  ,("ncursesw",                    ("sys-libs", "ncurses", Portage.AnySlot))
  ,("panel",                       ("sys-libs", "ncurses", Portage.AnySlot))
  ,("panelw",                      ("sys-libs", "ncurses", Portage.AnySlot))
  ]
