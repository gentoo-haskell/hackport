module Overlays
    ( getOverlayPath
    ) where

import Control.Monad
import Data.List (nub, inits)
import Data.Maybe (maybeToList, listToMaybe, isJust, fromJust)
import System.Directory
import System.FilePath ((</>), splitPath, joinPath)

import Error
import CacheFile
import Portage.Host

-- cabal
import Distribution.Verbosity
import Distribution.Simple.Utils ( info )

getOverlayPath :: Verbosity -> Maybe FilePath -> IO String
getOverlayPath verbosity override_overlay = do
  overlays <- if isJust override_overlay
                  then do info verbosity $ "Forced " ++ fromJust override_overlay
                          return [fromJust override_overlay]
                  else getOverlays
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
    mapM_ (info verbosity . (" * " ++)) mul
    info verbosity "Looking for one with a HackPort cache..."
    overlay <- loop mul
    info verbosity $ "I choose " ++ overlay
    info verbosity "Override my decision with hackport --overlay /my/overlay"
    return overlay

getOverlays :: IO [String]
getOverlays = do
  local    <- getLocalOverlay
  overlays <- overlay_list `fmap` getInfo
  return $ nub $ map clean $
                 maybeToList local
              ++ overlays
  where
  clean path = case reverse path of
                '/':p -> reverse p
                _ -> path

getLocalOverlay :: IO (Maybe FilePath)
getLocalOverlay = do
  curDir <- getCurrentDirectory
  let lookIn = map joinPath . reverse . inits . splitPath $ curDir
  fmap listToMaybe (filterM probe lookIn)

  where
    probe dir = doesDirectoryExist (dir </> "dev-haskell")

