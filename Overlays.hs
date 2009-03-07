module Overlays where

import Control.Monad
import Data.List (nub, inits)
import Data.Maybe (maybeToList, listToMaybe)
import System.Directory
import System.FilePath ((</>), splitPath, joinPath)

import Bash
import Error
import CacheFile

-- cabal
import Distribution.Verbosity
import Distribution.Simple.Utils ( info )

getOverlayPath :: Verbosity -> IO String
getOverlayPath verbosity = do
  overlays <- getOverlays
  case overlays of
    [] -> throwEx NoOverlay
    [x] -> return x
    mul -> search mul
  where
  search :: [String] -> IO String
  search mul = do
    let loop [] = throwEx (MultipleOverlays mul)
        loop (x:xs) = do
          info verbosity $ "Checking '" ++ x ++ "'..."
          found <- doesFileExist (cacheFile x)
          if found
            then do
              info verbosity "OK!"
              return x
            else do
              info verbosity "Not ok."
              loop xs
    info verbosity "There are several overlays in your configuration."
    mapM (info verbosity . (" * " ++)) mul
    info verbosity "Looking for one with a HackPort cache..."
    overlay <- loop mul
    info verbosity $ "I choose " ++ overlay
    info verbosity "Override my decision with hackport --overlay /my/overlay"
    return overlay

getOverlays :: IO [String]
getOverlays = do
  local   <- getLocalOverlay
  portage <- getGlobalPortageOverlays
  paludis <- getGlobalPaludisOverlays
  return $ nub $ maybeToList local
              ++ portage
              ++ paludis

getGlobalPortageOverlays :: IO [String]
getGlobalPortageOverlays =
  fmap words (runBash "source /etc/make.conf;echo -n $PORTDIR_OVERLAY")

getGlobalPaludisOverlays :: IO [String]
getGlobalPaludisOverlays = return [] -- TODO: fix

getLocalOverlay :: IO (Maybe FilePath)
getLocalOverlay = do
  curDir <- getCurrentDirectory
  let lookIn = map joinPath . reverse . inits . splitPath $ curDir
  fmap listToMaybe (filterM probe lookIn)

  where
    probe dir = doesDirectoryExist (dir </> "dev-haskell")

