module Portage.Overlay
  ( ExistingEbuild(..)
  , Overlay(..)
  , load, loadLazy
  , reduceOverlay
  , inOverlay
  )
  where

import qualified Portage.PackageId as Portage
import qualified Portage.Version   as Portage

import qualified Distribution.Package as Cabal

import qualified Distribution.Simple.PackageIndex as PackageIndex
-- import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Text (simpleParse, display)
import Distribution.Simple.Utils ( comparing, equating )

import Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.FilePath  ((</>), splitExtension)

--main = do
--  pkgs <- blingProgress . Progress.fromList . readOverlay
--      =<< getDirectoryTree "."
--  putStrLn $ unlines [ display pkg
--                     | pkg <- pkgs
--                     , isNothing (Portage.toCabalPackageId pkg) ]

--TODO: move this to another module:
data ExistingEbuild = ExistingEbuild {
    ebuildId      :: Portage.PackageId,
    ebuildCabalId :: Cabal.PackageIdentifier,
    ebuildPath    :: FilePath
  } deriving (Show,Ord,Eq)

instance Cabal.Package ExistingEbuild where packageId = ebuildCabalId

data Overlay = Overlay {
    overlayPath  :: FilePath,
    overlayMap :: Map Portage.PackageName [ExistingEbuild]
  } deriving Show

inOverlay :: Overlay -> Cabal.PackageId -> Bool
inOverlay overlay pkgId = not (Map.null packages)
  where
    packages = Map.filterWithKey
                (\(Portage.PackageName _cat overlay_pn) ebuilds -> 
                    let cabal_pn = Cabal.pkgName pkgId
                        ebs = [ ()
                                  | e <- ebuilds
                                  , let ebuild_cabal_id = ebuildCabalId e
                                  , ebuild_cabal_id == pkgId
                                  ]
                    in cabal_pn == overlay_pn && (not (null ebs))) om
    om = overlayMap overlay

load :: FilePath -> IO Overlay
load dir = fmap (mkOverlay . readOverlay) (getDirectoryTree dir)
  where
    mkOverlay packages = Overlay {
      overlayPath  = dir
--      TODO: ignore all ebuilds that have no Cabal version number
--      overlayIndex = PackageIndex.fromList packages
    }

loadLazy :: FilePath -> IO Overlay
loadLazy dir = fmap (mkOverlay . readOverlayByPackage) (getDirectoryTree dir)
  where
    allowed v = case v of
      (Portage.Version _ Nothing [] _) -> True
      _                                -> False
    a <-> b = a ++ '-':b
    a <.> b = a ++ '.':b

    mkOverlay :: [(Portage.PackageName, [Portage.Version])] -> Overlay
    mkOverlay packages = Overlay {
      overlayPath  = dir,
      overlayMap =
          Map.fromList
            [ (pkgName, [ ExistingEbuild portageId cabalId filepath
                        | version <- allowedVersions
                        , let portageId = Portage.PackageId pkgName version
                        , Just cabalId <- [ Portage.toCabalPackageId portageId ]
                        , let filepath =
                                dir </> display pkgName <-> display version <.> "ebuild"
                        ])
            | (pkgName, allVersions) <- packages
            , let allowedVersions = filter allowed allVersions
           ]
    }

-- make sure there is only one ebuild for each version number (by selecting
-- the highest ebuild version revision)
reduceOverlay :: Overlay -> Overlay
reduceOverlay overlay = overlay { overlayMap = Map.map reduceVersions (overlayMap overlay) }
  where
  versionNumbers (Portage.Version nums _ _ _) = nums
  reduceVersions :: [ExistingEbuild] -> [ExistingEbuild]
  reduceVersions ebuilds = -- gah!
          map (maximumBy (comparing (Portage.pkgVersion . ebuildId)))
          . groupBy (equating (versionNumbers . Portage.pkgVersion . ebuildId))
          . sortBy (comparing (Portage.pkgVersion . ebuildId))
          $ ebuilds

readOverlayByPackage :: DirectoryTree -> [(Portage.PackageName, [Portage.Version])]
readOverlayByPackage tree =
  [ (name, versions name pkgTree)
  | (category, catTree) <- categories        tree
  , (name,     pkgTree) <- packages category catTree
  ]

  where
    categories :: DirectoryTree -> [(Portage.Category, DirectoryTree)]
    categories entries =
      [ (category, entries')
      | Directory dir entries' <- entries
      , Just category <- [simpleParse dir] ]

    packages :: Portage.Category -> DirectoryTree
             -> [(Portage.PackageName, DirectoryTree)]
    packages category entries =
      [ (Portage.PackageName category name, entries')
      | Directory dir entries' <- entries
      , Just name <- [simpleParse dir] ]

    versions :: Portage.PackageName -> DirectoryTree -> [Portage.Version]
    versions name@(Portage.PackageName (Portage.Category category) _) entries =
      [ version
      | File fileName <- entries
      , let (baseName, ext) = splitExtension fileName
      , ext == ".ebuild"
      , let fullName = category ++ '/' : baseName
      , Just (Portage.PackageId name' version) <- [simpleParse fullName]
      , name == name' ]

readOverlay :: DirectoryTree -> [Portage.PackageId]
readOverlay tree = [ Portage.PackageId pkgId version
                   | (pkgId, versions) <- readOverlayByPackage tree
                   , version <- versions
                   ]

type DirectoryTree  = [DirectoryEntry]
data DirectoryEntry = File FilePath | Directory FilePath [DirectoryEntry]

getDirectoryTree :: FilePath -> IO DirectoryTree
getDirectoryTree = dirEntries

  where
    dirEntries :: FilePath -> IO [DirectoryEntry]
    dirEntries dir = do
      names <- getDirectoryContents dir
      sequence
        [ do isDirectory <- doesDirectoryExist path
             if isDirectory
               then do entries <- unsafeInterleaveIO (dirEntries path)
                       return (Directory name entries)
               else    return (File      name)
        | name <- names
        , not (ignore name)
        , let path = dir </> name ]

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False
