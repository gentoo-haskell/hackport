module Portage.Overlay where

import qualified Portage.PackageId as Portage

-- FIXME! use Portage.Version instead of Cabal version
import qualified Distribution.Version as Portage

import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.FilePath  ((</>))


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
    categories = error "TODO"

    packages :: Portage.Category -> DirectoryTree
             -> [(Portage.PackageName, DirectoryTree)]
    packages = error "TODO"

    versions :: Portage.PackageName -> DirectoryTree -> [Portage.Version]
    versions = error "TODO"

data DirectoryTree = File FilePath | Directory FilePath [DirectoryTree]

getDirectoryTree :: FilePath -> IO DirectoryTree
getDirectoryTree = dirTreeStrict

  where
    dirTreeStrict, dirTreeLazy :: FilePath -> IO DirectoryTree
    dirTreeStrict dir = fmap (Directory dir) (dirEntries dir)
    dirTreeLazy   dir = fmap (Directory dir) (unsafeInterleaveIO
                                               (dirEntries dir))

    dirEntries :: FilePath -> IO [DirectoryTree]
    dirEntries dir = do
      names <- getDirectoryContents dir
      sequence
        [ do isDirectory <- doesDirectoryExist entry
             if isDirectory
               then dirTreeLazy entry
               else return (File name)
        | name <- names
        , not (ignore name)
        , let entry = dir </> name ]

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False
