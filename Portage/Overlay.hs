module Portage.Overlay where

import qualified Portage.PackageId as Portage
import qualified Portage.Version   as Portage

import qualified Distribution.Package as Cabal

import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Text (simpleParse)

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
  }

instance Cabal.Package ExistingEbuild where packageId = ebuildCabalId

data Overlay = Overlay {
    overlayPath  :: FilePath

    --TODO:
--    overlayMap   :: Map Portage.PackageId ???
--      -- or perhaps a trie
--    overlayIndex :: PackageIndex ExistingEbuild
  }

load :: FilePath -> IO Overlay
load dir = fmap (mkOverlay . readOverlay) (getDirectoryTree dir)
  where
    mkOverlay packages = Overlay {
      overlayPath  = dir
--      TODO: ignore all ebuilds that have no Cabal version number
--      overlayIndex = PackageIndex.fromList packages
    }

readOverlay :: DirectoryTree -> [Portage.PackageId]
readOverlay tree =
  [ Portage.PackageId name version
  | (category, catTree) <- categories        tree
  , (name,     pkgTree) <- packages category catTree
  ,  version            <- versions name     pkgTree ]

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
