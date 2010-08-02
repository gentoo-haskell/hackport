module Portage.Overlay
  ( ExistingEbuild(..)
  , Overlay(..)
  , loadLazy
  , readOverlay, readOverlayByPackage
  , getDirectoryTree, DirectoryTree

  , reduceOverlay
  , filterByHerd
  , inOverlay
  )
  where

import qualified Portage.PackageId as Portage
import qualified Portage.Metadata as Portage

import qualified Distribution.Package as Cabal

import Distribution.Text (simpleParse, display)
import Distribution.Simple.Utils ( comparing, equating )

import Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.FilePath  ((</>), splitExtension)

import Data.Traversable ( traverse )

data ExistingEbuild = ExistingEbuild {
    ebuildId      :: Portage.PackageId,
    ebuildCabalId :: Cabal.PackageIdentifier,
    ebuildPath    :: FilePath
  } deriving (Show,Ord,Eq)

instance Cabal.Package ExistingEbuild where packageId = ebuildCabalId

data Overlay = Overlay {
    overlayPath  :: FilePath,
    overlayMap :: Map Portage.PackageName [ExistingEbuild],
    overlayMetadata :: Map Portage.PackageName Portage.Metadata
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

loadLazy :: FilePath -> IO Overlay
loadLazy path = do
  dir <- getDirectoryTree path
  metadata <- mkMetadataMap path dir
  return $ mkOverlay metadata $ readOverlayByPackage dir
  where
    allowed v = case v of
      (Portage.Version _ Nothing [] _) -> True
      _                                -> False
    a <-> b = a ++ '-':b
    a <.> b = a ++ '.':b

    mkOverlay :: Map Portage.PackageName Portage.Metadata
              -> [(Portage.PackageName, [Portage.Version])]
              -> Overlay
    mkOverlay meta packages = Overlay {
      overlayPath  = path,
      overlayMetadata = meta,
      overlayMap =
          Map.fromList
            [ (pkgName, [ ExistingEbuild portageId cabalId filepath
                        | version <- allowedVersions
                        , let portageId = Portage.PackageId pkgName version
                        , Just cabalId <- [ Portage.toCabalPackageId portageId ]
                        , let filepath = path </> display pkgName <-> display version <.> "ebuild"
                        ])
            | (pkgName, allVersions) <- packages
            , let allowedVersions = filter allowed allVersions
           ]
    }

mkMetadataMap :: FilePath -> DirectoryTree -> IO (Map Portage.PackageName Portage.Metadata)
mkMetadataMap root dir =
  fmap (Map.mapMaybe id) $
    traverse Portage.metadataFromFile $
      Map.fromList
        [ (Portage.mkPackageName category package, root </> category </> package </> "metadata.xml")
        | Directory category packages <- dir
        , Directory package files <- packages
        , File "metadata.xml" <- files
        ]

filterByHerd :: Overlay -> ([String] -> Bool) -> Overlay
filterByHerd overlay p = overlay
                            { overlayMetadata = metadataMap'
                            , overlayMap = pkgMap'
                            }
  where
    metadataMap' = Map.filter (p . Portage.metadataHerds) (overlayMetadata overlay)
    pkgMap' = Map.intersection (overlayMap overlay) metadataMap'


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
