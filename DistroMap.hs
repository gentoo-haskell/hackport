{-# OPTIONS -XPatternGuards #-}
{-
Generate a distromap, like these:
http://hackage.haskell.org/packages/archive/00-distromap/
Format:

("xmobar","0.8",Just "http://packages.gentoo.org/package/x11-misc/xmobar")
("xmobar","0.9",Just "http://packages.gentoo.org/package/x11-misc/xmobar")
("xmobar","0.9.2",Just "http://packages.gentoo.org/package/x11-misc/xmobar")
("xmonad","0.5",Just "http://packages.gentoo.org/package/x11-wm/xmonad")
("xmonad","0.6",Just "http://packages.gentoo.org/package/x11-wm/xmonad")
("xmonad","0.7",Just "http://packages.gentoo.org/package/x11-wm/xmonad")
("xmonad","0.8",Just "http://packages.gentoo.org/package/x11-wm/xmonad")
("xmonad","0.8.1",Just "http://packages.gentoo.org/package/x11-wm/xmonad")
("xmonad","0.9",Just "http://packages.gentoo.org/package/x11-wm/xmonad")
("xmonad","0.9.1",Just "http://en.gentoo-wiki.com/wiki/Haskell/overlay")

Multiple entries for each package is allowed, given that there are different versions.


Setup:
  Join all packages from portage and the overlay into a big map;
  From Portage.PackageId: PackageName = category/package
  PVULine = (packagename, versionstring, url)
  Create such a map: Map PackageName DistroLine
  Only one PVULine per version, and prefer portage over the overlay.

Algorithm;
  1. Take a package from hackage
  2. Look for it in the map
    a. For each version:
        find a match in the list of versions:
          yield the PVULine
-}

module DistroMap
  ( distroMap ) where

import Control.Applicative
import qualified Data.List as List ( nub )
import qualified Data.Map as Map
import Data.Map ( Map )
import System.FilePath ( (</>) )
import Debug.Trace ( trace )
import Data.Maybe ( fromJust )

import Distribution.Verbosity
import Distribution.Text ( display )
import Distribution.Client.Types ( Repo, AvailablePackageDb(..), AvailablePackage(..) )
import Distribution.Simple.Utils ( info, warn, notice )

import qualified Data.Version as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.Client.PackageIndex as CabalInstall
import qualified Distribution.Client.IndexUtils as CabalInstall

import Portage.Overlay ( Overlay(..), readOverlayByPackage, getDirectoryTree, DirectoryTree )
import qualified Portage.PackageId as Portage
import qualified Portage.Version as Portage

type PVU = (Cabal.PackageName, Cabal.Version, Maybe String)
type PVU_Map = Map Portage.PackageName [(Cabal.Version, Maybe String)]
type PVU_Item = (Portage.PackageName, [(Cabal.Version, Maybe String)])

distroMap :: Verbosity -> Repo -> FilePath -> FilePath -> [String] -> IO ()
distroMap verbosity repo portagePath overlayPath args = do
  putStrLn "distro map called"
  putStrLn ("verbosity: " ++ show verbosity)
  putStrLn ("portage: " ++ portagePath)
  putStrLn ("overlay: " ++ overlayPath)
  putStrLn ("args: " ++ show args)

  portage <- readOverlayByPackage <$> getDirectoryTree portagePath
  overlay <- readOverlayByPackage <$> getDirectoryTree overlayPath

  putStrLn ("portage packages: " ++ show (length portage))
  putStrLn ("overlay packages: " ++ show (length overlay))

  let portageMap = buildPortageMap portage
      overlayMap = buildOverlayMap overlay
      completeMap = unionMap portageMap overlayMap

  putStrLn ("portage map: " ++ show (Map.size portageMap))
  putStrLn ("overlay map: " ++ show (Map.size overlayMap))
  putStrLn ("complete map: " ++ show (Map.size completeMap))

  AvailablePackageDb { packageIndex = packageIndex } <-
    CabalInstall.getAvailablePackages verbosity [repo]

  let pkgs0 = map (map packageInfoId) (CabalInstall.allPackagesByName packageIndex)
      hackagePkgs = [ (Cabal.pkgName (head p), map Cabal.pkgVersion p) | p <- pkgs0 ]

  putStrLn ("cabal packages: " ++ show (length hackagePkgs))

  let pvus = concat $ map (\(p,vs) -> lookupPVU completeMap p vs) hackagePkgs
  putStrLn ("found pvus: " ++ show (length pvus))

  mapM_ (putStrLn . showPVU) pvus
  return ()


showPVU :: PVU -> String
showPVU (p,v,u) = show $ (display p, display v, u)

-- building the PVU_Map

reduceVersion :: Portage.Version -> Portage.Version
reduceVersion (Portage.Version ns _ _ _) = Portage.Version ns Nothing [] 0

reduceVersions :: [Portage.Version] -> [Portage.Version]
reduceVersions = List.nub . map reduceVersion

buildMap :: [(Portage.PackageName, [Portage.Version])]
         -> (Portage.PackageName -> Portage.Version -> Maybe String)
         -> PVU_Map
buildMap pvs f = Map.mapWithKey (\p vs -> [ (fromJust $ Portage.toCabalVersion v, f p v)
                                          | v <- reduceVersions vs ])
                                (Map.fromList pvs)

buildPortageMap :: [(Portage.PackageName, [Portage.Version])] -> PVU_Map
buildPortageMap lst = buildMap lst $ \ (Portage.PackageName c p) _v ->
  Just $ "http://packages.gentoo.org/package" </> display c </> display p

buildOverlayMap :: [(Portage.PackageName, [Portage.Version])] -> PVU_Map
buildOverlayMap lst = buildMap lst $ \_ _ -> Just "http://en.gentoo-wiki.com/wiki/Haskell/overlay"

unionMap :: PVU_Map -> PVU_Map -> PVU_Map
unionMap = Map.unionWith f
  where
  f :: [(Cabal.Version, Maybe String)]
    -> [(Cabal.Version, Maybe String)]
    -> [(Cabal.Version, Maybe String)]
  f vas vbs = Map.toList (Map.union (Map.fromList vas) (Map.fromList vbs))


-- resolving Cabal.PackageName to Portage.PackageName

resolveCategories :: PVU_Map
                  -> Cabal.PackageName
                  -> [PVU_Item]
resolveCategories pvu_map cpn = Map.toList $ Map.filterWithKey f pvu_map
  where
  f (Portage.PackageName _cat pn) _vs = cpn == pn

lookupPVU :: PVU_Map -> Cabal.PackageName -> [Cabal.Version] -> [PVU]
lookupPVU pvu_map pn cvs =
  case resolveCategories pvu_map (Portage.normalizeCabalPackageName pn) of
    [] -> []
    [item] -> ret item
    --items | (cat@(:_) <- (filter (`elem` cats) priority) -> ret cat
    --     | otherwise -> trace ("Ambiguous package name: " ++ show pn ++ ", hits: " ++ show cats) Nothing
    _ -> trace ("too many packages: " ++ show pn) []
  where
  ret (_, vs) = [ (pn, v, u) | (v, u) <- vs, v `elem` cvs ]
  preferableItem items =
    [ item
    | item@(Portage.PackageName cat _pn, _vs) <- items
    , cat == Portage.Category "dev-haskell"]
