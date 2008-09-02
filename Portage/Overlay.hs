module Portage.Overlay where

import qualified Portage.PackageId as Portage

-- FIXME! use Portage.Version instead of Cabal version
import qualified Distribution.Version as Portage

import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Text (simpleParse)

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.FilePath  ((</>), splitExtension)

--main = putStrLn . unlines . map display
--   =<< blingProgress . Progress.fromList . readOverlay
--   =<< getDirectoryTree "."

data Overlay = Overlay {
    overlayPath  :: FilePath,
    overlayIndex :: PackageIndex Portage.PackageId
  }

load :: FilePath -> IO Overlay
load dir = fmap (mkOverlay . readOverlay) (getDirectoryTree dir)
  where
    mkOverlay packages = Overlay {
      overlayPath  = dir,
      overlayIndex = PackageIndex.fromList packages
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
