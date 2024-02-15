{-|
Module      : Merge.Dependencies
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Merge a package from @hackage@ to an ebuild.
-}

{-# LANGUAGE CPP #-}

-- TODO: Rearrange things so we don't have to disable this warning
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Merge.Dependencies
  ( EDep(..)
  , RetroPackageDescription(..)
  , exeAndLibDeps
  , mkRetroPD
  , resolveDependencies
  ) where

import           Control.DeepSeq (NFData(..))
import           Data.Default.Class
import           Data.Maybe ( isJust, isNothing )
import qualified Data.List as L
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup (Semigroup(..))
#endif
import qualified Data.Set  as S

import qualified Distribution.CabalSpecVersion as Cabal
import qualified Distribution.Compat.NonEmptySet as NES
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Version as Cabal
import qualified Distribution.Pretty as Cabal

import qualified Distribution.Compiler as Cabal

import qualified Portage.Cabal as Portage
import qualified Portage.Dependency as Portage
import qualified Portage.Dependency.Normalize as PN
import qualified Portage.Overlay as Portage
import qualified Portage.PackageId as Portage
import qualified Portage.Use as Portage
import qualified Portage.Slots as Portage
import qualified Cabal2Ebuild as C2E

import qualified Portage.GHCCore as GHCCore

import Debug.Trace ( trace )

-- | Dependencies of an ebuild.
data EDep = EDep
  {
    rdep :: Portage.Dependency,
    rdep_e :: S.Set String,
    dep :: Portage.Dependency,
    dep_e :: S.Set String
  }
  deriving (Show, Eq, Ord)

instance NFData EDep where
  rnf (EDep rd rde d de) = rnf rd `seq` rnf rde `seq` rnf d `seq` rnf de

-- | Cabal-1 style 'Cabal.PackageDescription', with a top-level 'buildDepends' function.
data RetroPackageDescription = RetroPackageDescription {
  packageDescription :: Cabal.PackageDescription,
  buildDepends :: [Cabal.Dependency]
  } deriving (Show)

-- | Construct a 'RetroPackageDescription' using 'exeAndLibDeps' for the 'buildDepends'.
mkRetroPD :: Cabal.PackageDescription -> RetroPackageDescription
mkRetroPD pd = RetroPackageDescription { packageDescription = pd, buildDepends = exeAndLibDeps pd }

-- | Extract only the build dependencies for libraries and executables for a given package.
exeAndLibDeps :: Cabal.PackageDescription -> [Cabal.Dependency]
exeAndLibDeps pkg = concatMap (Cabal.targetBuildDepends . Cabal.buildInfo)
                    (Cabal.executables pkg)
                    `L.union`
                    concatMap (Cabal.targetBuildDepends . Cabal.libBuildInfo)
                    (Cabal.allLibraries pkg)

instance Semigroup EDep where
  (EDep rdepA rdep_eA depA dep_eA) <> (EDep rdepB rdep_eB depB dep_eB) = EDep
    { rdep   = Portage.DependAllOf [rdepA, rdepB]
    , rdep_e = rdep_eA `S.union` rdep_eB
    , dep    = Portage.DependAllOf [depA, depB]
    , dep_e  = dep_eA  `S.union` dep_eB
    }
  
instance Monoid EDep where
  mempty = EDep
      {
        rdep = Portage.empty_dependency,
        rdep_e = S.empty,
        dep = Portage.empty_dependency,
        dep_e = S.empty
      }
#if !(MIN_VERSION_base(4,11,0))
  (EDep rdepA rdep_eA depA dep_eA) `mappend` (EDep rdepB rdep_eB depB dep_eB) = EDep
    { rdep   = Portage.DependAllOf [rdepA, rdepB]
    , rdep_e = rdep_eA `S.union` rdep_eB
    , dep    = Portage.DependAllOf [depA, depB]
    , dep_e  = dep_eA  `S.union` dep_eB
    }
#endif

-- | Resolve package dependencies from a 'RetroPackageDescription' into an 'EDep'.
resolveDependencies :: Portage.Overlay -> RetroPackageDescription -> Cabal.CompilerInfo
                    -> [Cabal.PackageName] -> Cabal.PackageName
                    -> EDep
resolveDependencies overlay pkg compiler_info ghc_package_names merged_cabal_pkg_name = edeps
  where
    -- hasBuildableExes p = any (buildable . buildInfo) . executables $ p
    treatAsLibrary :: Bool
    treatAsLibrary = isJust (Cabal.library (packageDescription pkg))
    -- without slot business
    raw_haskell_deps :: Portage.Dependency
    raw_haskell_deps = PN.normalize_depend $ Portage.DependAllOf $ haskellDependencies overlay (buildDepends pkg)
    test_deps :: Portage.Dependency
    test_deps = Portage.mkUseDependency (True, Portage.Use "test") $
                    Portage.DependAllOf $
                    remove_raw_common $
                    testDependencies overlay (packageDescription pkg) ghc_package_names merged_cabal_pkg_name
    cabal_dep :: Portage.Dependency
    cabal_dep = cabalDependency overlay (packageDescription pkg) compiler_info
    ghc_dep :: Portage.Dependency
    ghc_dep = compilerInfoToDependency compiler_info
    extra_libs :: Portage.Dependency
    extra_libs = Portage.DependAllOf $ findCLibs (packageDescription pkg)
    pkg_config_libs :: [Portage.Dependency]
    pkg_config_libs = pkgConfigDependencies overlay (packageDescription pkg)
    pkg_config_tools :: Portage.Dependency
    pkg_config_tools = Portage.DependAllOf $ if L.null pkg_config_libs
                           then []
                           else [any_c_p "virtual" "pkgconfig"]
    build_tools :: Portage.Dependency
    build_tools = Portage.DependAllOf $ pkg_config_tools : legacyBuildToolsDependencies (packageDescription pkg)
                  ++ hackageBuildToolsDependencies overlay (packageDescription pkg)

    setup_deps :: Portage.Dependency
    setup_deps = PN.normalize_depend $ Portage.DependAllOf $
                     remove_raw_common $
                     setupDependencies overlay (packageDescription pkg) ghc_package_names merged_cabal_pkg_name

    edeps :: EDep
    edeps
        | treatAsLibrary = mempty
                  {
                    dep = Portage.DependAllOf
                              [ cabal_dep
                              , setup_deps
                              , build_tools
                              , test_deps
                              ],
                    dep_e = S.singleton "${RDEPEND}",
                    rdep = Portage.DependAllOf
                               [ Portage.set_build_slot ghc_dep
                               , Portage.set_build_slot $ add_profile $ raw_haskell_deps
                               , extra_libs
                               , Portage.DependAllOf pkg_config_libs
                               ]
                  }
        | otherwise = mempty
                  {
                    dep = Portage.DependAllOf
                              [ cabal_dep
                              , setup_deps
                              , build_tools
                              , test_deps
                              ],
                    dep_e = S.singleton "${RDEPEND}",
                    rdep = Portage.DependAllOf
                               [ Portage.set_build_slot ghc_dep
                               , Portage.set_build_slot $ raw_haskell_deps
                               , extra_libs
                               , Portage.DependAllOf pkg_config_libs
                               ]
                  }
    add_profile    = Portage.addDepUseFlag (Portage.mkQUse (Portage.Use "profile"))
    -- remove depends present in common section
    remove_raw_common = filter (\d -> not (Portage.dep_as_broad_as d raw_haskell_deps))
                        . map PN.normalize_depend

---------------------------------------------------------------
-- Custom-setup dependencies
-- TODO: move partitioning part to Merge:mergeGenericPackageDescription
---------------------------------------------------------------

setupDependencies :: Portage.Overlay -> Cabal.PackageDescription -> [Cabal.PackageName] -> Cabal.PackageName -> [Portage.Dependency]
setupDependencies overlay pkg ghc_package_names merged_cabal_pkg_name = deps
    where cabalDeps = maybe [] id $ Cabal.setupDepends `fmap` Cabal.setupBuildInfo pkg
          cabalDeps' = fst $ Portage.partition_depends ghc_package_names merged_cabal_pkg_name cabalDeps
          deps = C2E.convertDependencies overlay (Portage.Category "dev-haskell") cabalDeps'

---------------------------------------------------------------
-- Test-suite dependencies
-- TODO: move partitioning part to Merge:mergeGenericPackageDescription
---------------------------------------------------------------

testDependencies :: Portage.Overlay -> Cabal.PackageDescription -> [Cabal.PackageName] -> Cabal.PackageName -> [Portage.Dependency]
testDependencies overlay pkg ghc_package_names merged_cabal_pkg_name = deps
    where cabalDeps = concatMap (Cabal.targetBuildDepends . Cabal.testBuildInfo) (Cabal.testSuites pkg)
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
cabalDependency :: Portage.Overlay -> Cabal.PackageDescription -> Cabal.CompilerInfo -> Portage.Dependency
cabalDependency overlay pkg ~(Cabal.CompilerInfo {
                                  Cabal.compilerInfoId =
                                      Cabal.CompilerId Cabal.GHC cabal_version
                              }) =
         C2E.convertDependency overlay
                               (Portage.Category "dev-haskell")
                               (Cabal.Dependency (Cabal.mkPackageName "Cabal")
                                                 finalCabalDep (NES.singleton Cabal.defaultLibName))
  where
    versionNumbers = Cabal.versionNumbers cabal_version
    userCabalVersion = Cabal.orLaterVersion $ Cabal.mkVersion
                       $ Cabal.cabalSpecToVersionDigits $ Cabal.specVersion pkg
    shippedCabalVersion = GHCCore.cabalFromGHC versionNumbers
    shippedCabalDep = maybe Cabal.anyVersion Cabal.orLaterVersion shippedCabalVersion
    finalCabalDep = Cabal.simplifyVersionRange
                                (Cabal.intersectVersionRanges
                                          userCabalVersion
                                          shippedCabalDep)

---------------------------------------------------------------
-- GHC Dependency
---------------------------------------------------------------

compilerInfoToDependency :: Cabal.CompilerInfo -> Portage.Dependency
compilerInfoToDependency ~(Cabal.CompilerInfo {
                               Cabal.compilerInfoId =
                                   Cabal.CompilerId Cabal.GHC cabal_version}) =
  at_least_c_p_v "dev-lang" "ghc" (Cabal.versionNumbers cabal_version)

---------------------------------------------------------------
-- C Libraries
---------------------------------------------------------------

findCLibs :: Cabal.PackageDescription -> [Portage.Dependency]
findCLibs (Cabal.PackageDescription { Cabal.library = lib, Cabal.executables = exes }) =
  [ trace ("WARNING: This package depends on a C library we don't know the portage name for: " ++ p ++ ". Check the generated ebuild.")
          (any_c_p "unknown-c-lib" p)
  | p <- notFound
  ] ++
  found
  where
  libE = concatMap (Cabal.extraLibs . Cabal.libBuildInfo) $ maybe [] return lib
  exeE = concatMap Cabal.extraLibs (filter Cabal.buildable (map Cabal.buildInfo exes))
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
      , ("pq", at_least_c_p_v "dev-db" "postgresql" [7])
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
      , ("stdc++", Portage.DependAnyOf
            [ any_c_p_s_u "sys-devel" "gcc" Portage.AnySlot [Portage.mkUse (Portage.Use "cxx")]
            , any_c_p "sys-devel" "clang"
            ])
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
      , ("nanomsg", any_c_p "dev-libs" "nanomsg")
      , ("pgf", any_c_p "media-libs" "libpgf")
      , ("ssh2", any_c_p "net-libs" "libssh2")
      , ("dl", any_c_p "virtual" "libc")
      , ("glfw", any_c_p "media-libs" "glfw")
      , ("nettle", any_c_p "dev-libs" "nettle")
      , ("Xpm",    any_c_p "x11-libs" "libXpm")
      , ("Xss",    any_c_p "x11-libs" "libXScrnSaver")
      , ("tag_c",  any_c_p "media-libs" "taglib")
      , ("magic",  any_c_p "sys-apps" "file")
      , ("crypt",  any_c_p_s_u "virtual" "libcrypt" Portage.AnyBuildTimeSlot [])
      , ("Xrender", any_c_p "x11-libs" "libXrender")
      , ("Xcursor", any_c_p "x11-libs" "libXcursor")
      , ("Xinerama", any_c_p "x11-libs" "libXinerama")
      , ("wayland-client", any_c_p "dev-libs" "wayland")
      , ("wayland-cursor", any_c_p "dev-libs" "wayland")
      , ("wayland-server", any_c_p "dev-libs" "wayland")
      , ("wayland-egl", any_c_p_s_u "media-libs" "mesa" Portage.AnySlot [Portage.mkUse (Portage.Use "wayland")])
      , ("xkbcommon", any_c_p "x11-libs" "libxkbcommon")
      , ("SDL_gfx", any_c_p "media-libs" "sdl-gfx")
      , ("SDL_image", any_c_p "media-libs" "sdl-image")
      , ("SDL_ttf", any_c_p "media-libs" "sdl-ttf")
      , ("odbc", any_c_p "dev-db" "unixODBC")
      , ("uuid", any_c_p "sys-apps" "util-linux")
      , ("notify", any_c_p "x11-libs" "libnotify")
      , ("SDL2", any_c_p " media-libs" "libsdl2")
      , ("SDL2_mixer", any_c_p "media-libs" "sdl2-mixer")
      , ("blas", any_c_p "virtual" "blas")
      , ("lapack", any_c_p "virtual" "lapack")
      , ("libbrotlidec", any_c_p "app-arch" "brotli")
      , ("libbrotlienc", any_c_p "app-arch" "brotli")
      , ("libarchive", at_least_c_p_v "app-arch" "libarchive" [3,5,2])
      ]

---------------------------------------------------------------
-- Build Tools (legacy, a list of well-known tools)
---------------------------------------------------------------

legacyBuildToolsDependencies :: Cabal.PackageDescription -> [Portage.Dependency]
legacyBuildToolsDependencies (Cabal.PackageDescription { Cabal.library = lib, Cabal.executables = exes }) = L.nub $
  [ case pkg of
      Just p -> p
      Nothing -> trace ("WARNING: Unknown build tool '" ++ Cabal.prettyShow exe ++ "'. Check the generated ebuild.")
                       (any_c_p "unknown-build-tool" pn)
  | exe@(Cabal.LegacyExeDependency pn _range) <- cabalDeps
  , pkg <- return (lookup pn buildToolsTable)
  ]
  where
  cabalDeps = filter notProvided $ depL ++ depE
  depL = concatMap (Cabal.buildTools . Cabal.libBuildInfo) $ maybe [] return lib
  depE = concatMap Cabal.buildTools (filter Cabal.buildable (map Cabal.buildInfo exes))
  notProvided (Cabal.LegacyExeDependency pn _range) = pn `notElem` buildToolsProvided

buildToolsTable :: [(String, Portage.Dependency)]
buildToolsTable =
  [ ("happy", any_c_p "dev-haskell" "happy")
  , ("alex", any_c_p "dev-haskell" "alex")
  , ("c2hs", any_c_p "dev-haskell" "c2hs")
  , ("cabal",               any_c_p "dev-haskell" "cabal-install")
  , ("cabal-install", any_c_p "dev-haskell" "cabal-install")
  , ("cpphs",               any_c_p "dev-haskell" "cpphs")
  , ("ghc",                 any_c_p "dev-lang" "ghc")
  , ("gtk2hsTypeGen",       any_c_p "dev-haskell" "gtk2hs-buildtools")
  , ("gtk2hsHookGenerator", any_c_p "dev-haskell" "gtk2hs-buildtools")
  , ("gtk2hsC2hs",          any_c_p "dev-haskell" "gtk2hs-buildtools")
  , ("hsb2hs",              any_c_p "dev-haskell" "hsb2hs")
  , ("hsx2hs",              any_c_p "dev-haskell" "hsx2hs")
  , ("llvm-config",         any_c_p "sys-devel" "llvm")
  ]

-- tools that are provided by ghc or some other existing program
-- so we do not need dependencies on them
buildToolsProvided :: [String]
buildToolsProvided = ["hsc2hs"]

---------------------------------------------------------------
-- Hackage Build Tools (behind `build-tool-depends`)
---------------------------------------------------------------

hackageBuildToolsDependencies :: Portage.Overlay -> Cabal.PackageDescription -> [Portage.Dependency]
hackageBuildToolsDependencies overlay (Cabal.PackageDescription { Cabal.library = lib, Cabal.executables = exes }) =
  haskellDependencies overlay $ L.nub $
    [ Cabal.Dependency pn versionRange $ NES.singleton Cabal.defaultLibName
    | Cabal.ExeDependency pn _component versionRange <- cabalDeps
    ]
  where
    cabalDeps = depL ++ depE
    depL = concatMap (Cabal.buildToolDepends . Cabal.libBuildInfo) $ maybe [] return lib
    depE = concatMap Cabal.buildToolDepends (filter Cabal.buildable (map Cabal.buildInfo exes))

---------------------------------------------------------------
-- pkg-config
---------------------------------------------------------------

pkgConfigDependencies :: Portage.Overlay -> Cabal.PackageDescription -> [Portage.Dependency]
pkgConfigDependencies overlay (Cabal.PackageDescription { Cabal.library = lib, Cabal.executables = exes }) = L.nub $ resolvePkgConfigs overlay cabalDeps
  where
  cabalDeps = depL ++ depE
  depL = concatMap (Cabal.pkgconfigDepends . Cabal.libBuildInfo) $ maybe [] return lib
  depE = concatMap Cabal.pkgconfigDepends (filter Cabal.buildable (map Cabal.buildInfo exes))

resolvePkgConfigs :: Portage.Overlay -> [Cabal.PkgconfigDependency] -> [Portage.Dependency]
resolvePkgConfigs overlay cdeps =
  [ case resolvePkgConfig overlay pkg of
      Just d -> d
      Nothing -> trace ("WARNING: Could not resolve pkg-config: " ++ Cabal.prettyShow pkg ++ ". Check generated ebuild.")
                       (any_c_p "unknown-pkg-config" pn)
  | pkg@(Cabal.PkgconfigDependency cabal_pn _range) <- cdeps
  , let pn = Cabal.unPkgconfigName cabal_pn
  ]

resolvePkgConfig :: Portage.Overlay -> Cabal.PkgconfigDependency -> Maybe Portage.Dependency
resolvePkgConfig _overlay (Cabal.PkgconfigDependency cabal_pn _cabalVersion) = do
  (cat, portname, Portage.DAttr slot usedep) <- lookup (Cabal.unPkgconfigName cabal_pn) pkgconfig_table
  return $ any_c_p_s_u cat portname slot usedep

pkgconfig_table :: [(String, (String, String, Portage.DAttr))]
pkgconfig_table =
  [
   ("alsa",         ("media-libs", "alsa-lib", def))
  ,("atk",          ("app-accessibility", "at-spi2-core", Portage.DAttr (Portage.AnySlot) [Portage.mkUse $ Portage.Use "introspection"]))
  ,("gconf-2.0",    ("gnome-base", "gconf", def))

  ,("gio-2.0",                ("dev-libs", "glib", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gio-unix-2.0",           ("dev-libs", "glib", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("glib-2.0",               ("dev-libs", "glib", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gmodule-2.0",            ("dev-libs", "glib", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gmodule-export-2.0",     ("dev-libs", "glib", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gmodule-no-export-2.0",  ("dev-libs", "glib", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gobject-2.0",            ("dev-libs", "glib", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gobject-introspection-1.0", ("dev-libs", "gobject-introspection",
                                  def))
  ,("gthread-2.0",            ("dev-libs", "glib", Portage.DAttr (Portage.GivenSlot "2") []))

  ,("gtk+-2.0",            ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gdk-2.0",             ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gdk-3.0",             ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "3") []))
  ,("gdk-pixbuf-2.0",      ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gdk-pixbuf-xlib-2.0", ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gdk-x11-2.0",         ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gtk+-unix-print-2.0", ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gtk+-x11-2.0",        ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "2") []))

  ,("gtk+-3.0",            ("x11-libs", "gtk+", Portage.DAttr (Portage.GivenSlot "3") []))
  ,("webkitgtk-3.0",       ("net-libs", "webkit-gtk", Portage.DAttr (Portage.GivenSlot "3") []))

  ,("gtk4",                ("gui-libs", "gtk", Portage.DAttr (Portage.GivenSlot "4") [Portage.mkUse $ Portage.Use "introspection"]))
  ,("gtk4-x11",            ("gui-libs", "gtk", Portage.DAttr (Portage.GivenSlot "4") [Portage.mkUse $ Portage.Use "introspection", Portage.mkUse $ Portage.Use "X"]))

  ,("cairo",            ("x11-libs", "cairo", def))
  ,("cairo-gobject",    ("x11-libs", "cairo", Portage.DAttr (Portage.AnySlot) [Portage.mkUse $ Portage.Use "glib"])) -- need [glib] for dev-haskell/cairo
  ,("cairo-ft",         ("x11-libs", "cairo", def))
  ,("cairo-ps",         ("x11-libs", "cairo", def))
  ,("cairo-png",        ("x11-libs", "cairo", def))
  ,("cairo-pdf",        ("x11-libs", "cairo", def))
  ,("cairo-svg",        ("x11-libs", "cairo", def))
  ,("cairo-xlib",         ("x11-libs", "cairo", def))
  ,("cairo-xlib-xrender", ("x11-libs", "cairo", def))

  ,("javascriptcoregtk-4.0",   ("net-libs", "webkit-gtk", Portage.DAttr (Portage.GivenSlot "4") []))
  ,("webkit2gtk-4.0",          ("net-libs", "webkit-gtk", Portage.DAttr (Portage.GivenSlot "4") []))

  ,("pangocairo",       ("x11-libs", "pango", def))
  ,("pangoft2",         ("x11-libs", "pango", def))
  ,("pango",            ("x11-libs", "pango", Portage.DAttr (Portage.AnySlot) [Portage.mkUse $ Portage.Use "introspection"]))
  ,("pangoxft",         ("x11-libs", "pango", def))
  ,("pangox",           ("x11-libs", "pango", def))

  ,("libglade-2.0", ("gnome-base", "libglade", def))
  ,("libsoup-2.4",   ("net-libs", "libsoup", Portage.DAttr (Portage.GivenSlot "2.4") []))
  ,("gnome-vfs-2.0", ("gnome-base", "gnome-vfs", def))
  ,("gnome-vfs-module-2.0", ("gnome-base", "gnome-vfs", def))
  ,("webkit-1.0", ("net-libs","webkit-gtk", Portage.DAttr (Portage.GivenSlot "2") []))
  ,("gtksourceview-3.0", ("x11-libs", "gtksourceview", Portage.DAttr (Portage.GivenSlot "3.0") []))

  ,("gstreamer-0.10",              ("media-libs", "gstreamer", def))
  ,("gstreamer-base-0.10",         ("media-libs", "gstreamer", def))
  ,("gstreamer-check-0.10",        ("media-libs", "gstreamer", def))
  ,("gstreamer-controller-0.10",   ("media-libs", "gstreamer", def))
  ,("gstreamer-dataprotocol-0.10", ("media-libs", "gstreamer", def))
  ,("gstreamer-net-0.10",          ("media-libs", "gstreamer", def))

  ,("gstreamer-app-0.10",          ("media-libs", "gst-plugins-base", def))
  ,("gstreamer-audio-0.10",        ("media-libs", "gst-plugins-base", def))
  ,("gstreamer-video-0.10",        ("media-libs", "gst-plugins-base", def))
  ,("gstreamer-plugins-base-0.10", ("media-libs", "gst-plugins-base", def))

  ,("gtksourceview-2.0",           ("x11-libs", "gtksourceview", Portage.DAttr (Portage.GivenSlot "2.0") []))
  ,("librsvg-2.0",                 ("gnome-base","librsvg", def))
  ,("vte",                         ("x11-libs","vte", Portage.DAttr (Portage.GivenSlot "0") []))
  ,("gtkglext-1.0",                ("x11-libs","gtkglext", def))

  ,("curl",                        ("net-misc", "curl", def))
  ,("libxml2",                     ("dev-libs", "libxml2", def))
  ,("libgsasl",                    ("net-misc", "gsasl", def))
  ,("libzip",                      ("dev-libs", "libzip", def))
  ,("gnutls",                      ("net-libs", "gnutls", def))
  ,("libidn",                      ("net-dns", "libidn", def))
  ,("libxml-2.0",                  ("dev-libs", "libxml2", def))
  ,("yaml-0.1",                    ("dev-libs", "libyaml", def))
  ,("QtCore",                      ("dev-qt", "qtcore", def))
  ,("lua",                         ("dev-lang", "lua", def))
  ,("QtDeclarative",               ("dev-qt", "qtdeclarative", def))
  ,("QtGui",                       ("dev-qt", "qtgui", def))
  ,("QtOpenGL",                    ("dev-qt", "qtopengl", def))
  ,("QtScript",                    ("dev-qt", "qtscript", def))
  ,("ImageMagick",                 ("media-gfx", "imagemagick", def))
  ,("MagickWand",                  ("media-gfx", "imagemagick", def))
  ,("ncurses",                     ("sys-libs", "ncurses", def))
  ,("ncursesw",                    ("sys-libs", "ncurses", def))
  ,("panel",                       ("sys-libs", "ncurses", def))
  ,("panelw",                      ("sys-libs", "ncurses", def))
  ,("libssh2",                     ("net-libs", "libssh2", def))
  ,("SDL_image",                   ("media-libs", "sdl-image", def))
  ,("libzmq",                      ("net-libs", "zeromq", def))
  ,("taglib_c",                    ("media-libs", "taglib", def))
  ,("libcurl",                     ("net-misc", "curl", def))
  ,("libpq",                       ("dev-db", "postgresql", def))
  ,("poppler-glib",                ("app-text", "poppler", def))
  ,("gsl",                         ("sci-libs", "gsl", def))
  ,("libvirt",                     ("app-emulation", "libvirt", def))

  ,("Qt5Core",                     ("dev-qt", "qtcore", Portage.DAttr (Portage.GivenSlot "5") []))
  ,("Qt5Gui",                      ("dev-qt", "qtgui", Portage.DAttr (Portage.GivenSlot "5") []))
  ,("Qt5Qml",                      ("dev-qt", "qtdeclarative", Portage.DAttr (Portage.GivenSlot "5") []))
  ,("Qt5Quick",                    ("dev-qt", "qtdeclarative", Portage.DAttr (Portage.GivenSlot "5") []))
  ,("Qt5Widgets",                  ("dev-qt", "qtwidgets", Portage.DAttr (Portage.GivenSlot "5") []))

  ,("sdl2",                        ("media-libs", "libsdl2", def))
  ,("SDL2_image",                  ("media-libs", "sdl2-image", def))
  ,("SDL2_mixer",                  ("media-libs", "sdl2-mixer", def))
  ,("zlib",                        ("sys-libs", "zlib", def))
  ,("libpcre",                     ("dev-libs", "libpcre", def))
  ,("graphene-gobject-1.0",        ("media-libs", "graphene", Portage.DAttr (Portage.AnySlot) [Portage.mkUse $ Portage.Use "introspection"]))
  ,("harfbuzz",                    ("media-libs", "harfbuzz", def))
  ,("harfbuzz-gobject",            ("media-libs", "harfbuzz", Portage.DAttr (Portage.AnySlot) [Portage.mkUse $ Portage.Use "introspection"]))
  ,("x11",                         ("x11-libs", "libX11", def))
  ]
