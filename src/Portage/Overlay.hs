module Portage.Overlay
  ( ExistingEbuild(..)
  , Overlay(..)
  , loadLazy
  , readOverlay, readOverlayByPackage
  , getDirectoryTree, DirectoryTree

  , reduceOverlay
  , filterByEmail
  , inOverlay
  )
  where

import qualified Portage.PackageId as Portage
import qualified Portage.Metadata as Portage

import qualified Distribution.Package as Cabal

import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Utils ( comparing, equating )

import Control.Monad.IO.Class
import Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.FilePath  ((</>), splitExtension)

data ExistingEbuild = ExistingEbuild {
    ebuildId      :: Portage.PackageId,
    ebuildCabalId :: Cabal.PackageIdentifier,
    ebuildPath    :: FilePath
  } deriving (Show,Ord,Eq)

instance Cabal.Package ExistingEbuild where
    packageId = ebuildCabalId

instance Cabal.HasUnitId ExistingEbuild where
    installedUnitId _ = error "Portage.Cabal.installedUnitId: FIXME: should not be used"

-- | Type describing an overlay.
data Overlay = Overlay {
    overlayPath  :: FilePath,
    overlayMap :: Map Portage.PackageName [ExistingEbuild],
    overlayMetadata :: Map Portage.PackageName Portage.Metadata
  } deriving Show

-- | Is 'Cabal.PackageId' found in 'Overlay'?
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

loadLazy :: MonadIO m => FilePath -> m Overlay
loadLazy path = do
  dir <- getDirectoryTree path
  metadata <- liftIO $ unsafeInterleaveIO $ mkMetadataMap path dir
  return $ mkOverlay metadata $ readOverlayByPackage dir
  where
    allowed v = case v of
      (Portage.Version _ Nothing [] _) -> True
      _                                -> False

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
                        , let filepath = path </> Portage.packageIdToFilePath portageId
                        ])
            | (pkgName, allVersions) <- packages
            , let allowedVersions = filter allowed allVersions
           ]
    }

mkMetadataMap :: FilePath -> DirectoryTree -> IO (Map Portage.PackageName Portage.Metadata)
mkMetadataMap root dir =
  fmap (Map.mapMaybe id) $
    traverse Portage.readMetadataFile $
      Map.fromList
        [ (Portage.mkPackageName category package, root </> category </> package </> "metadata.xml")
        | Directory category packages <- dir
        , Directory package files <- packages
        , File "metadata.xml" <- files
        ]

filterByEmail :: ([String] -> Bool) -> Overlay -> Overlay
filterByEmail p overlay = overlay
                            { overlayMetadata = metadataMap'
                            , overlayMap = pkgMap'
                            }
  where
    metadataMap' = Map.filter (p . Portage.metadataEmails) (overlayMetadata overlay)
    pkgMap' = Map.intersection (overlayMap overlay) metadataMap'


-- | Make sure there is only one ebuild for each version number (by selecting
-- the highest ebuild version revision)
reduceOverlay :: Overlay -> Overlay
reduceOverlay overlay = overlay { overlayMap = Map.map reduceVersions (overlayMap overlay) }
  where
  versionNumbers (Portage.Version nums _ _ _) = nums
  reduceVersions :: [ExistingEbuild] -> [ExistingEbuild]
  reduceVersions = -- gah!
          map (maximumBy (comparing (Portage.pkgVersion . ebuildId)))
          . groupBy (equating (versionNumbers . Portage.pkgVersion . ebuildId))
          . sortOn (Portage.pkgVersion . ebuildId)

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
      , Just category <- [simpleParsec dir] ]

    packages :: Portage.Category -> DirectoryTree
             -> [(Portage.PackageName, DirectoryTree)]
    packages category entries =
      [ (Portage.PackageName category name, entries')
      | Directory dir entries' <- entries
      , Just name <- [simpleParsec dir] ]

    versions :: Portage.PackageName -> DirectoryTree -> [Portage.Version]
    versions name@(Portage.PackageName (Portage.Category category) _) entries =
      [ version
      | File fileName <- entries
      , let (baseName, ext) = splitExtension fileName
      , ext == ".ebuild"
      , let fullName = category ++ '/' : baseName
      , Just (Portage.PackageId name' version) <- [simpleParsec fullName]
      , name == name' ]

readOverlay :: DirectoryTree -> [Portage.PackageId]
readOverlay tree = [ Portage.PackageId pkgId version
                   | (pkgId, versions) <- readOverlayByPackage tree
                   , version <- versions
                   ]

type DirectoryTree  = [DirectoryEntry]
data DirectoryEntry = File FilePath | Directory FilePath [DirectoryEntry]

getDirectoryTree :: MonadIO m => FilePath -> m DirectoryTree
getDirectoryTree = liftIO . dirEntries

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

    ignore path = path `elem` [ "."
                              , ".."
                              -- those speed things up a bit
                              -- and reduse memory consumption
                              -- (as we store it in RAM for the whole run)
                              , ".git"
                              , "CVS"
                              ]
