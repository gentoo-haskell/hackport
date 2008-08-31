module Overlays where

import Control.Monad
import System.Directory
import Data.List (nub)

import Bash
import Error
import CacheFile

-- cabal
import Distribution.Verbosity
import Distribution.Simple.Utils ( info )

getOverlayPath :: Verbosity -> IO String
getOverlayPath verbose = do
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
          info verbose $ "Checking '" ++ x ++ "'..."
          found <- doesFileExist (cacheFile x)
          if found
            then do
              info verbose "OK!"
              return x
            else do
              info verbose "Not ok." 
              loop xs
    info verbose "There are several overlays in your configuration."
    mapM (info verbose . (" * " ++)) mul
    info verbose "Looking for one with a HackPort cache..."
    overlay <- loop mul
    info verbose $ "I choose " ++ overlay
    info verbose "Override my decision with hackport -p /my/overlay"
    return overlay

portageOverlays :: IO [String]
portageOverlays = runBash "source /etc/make.conf;echo -n $PORTDIR_OVERLAY" >>= (return.words)

paludisOverlays :: IO [String]
paludisOverlays = return [] -- TODO: fix

getOverlays :: IO [String]
getOverlays = do
  portage <- portageOverlays
  paludis <- paludisOverlays
  return (nub (portage ++ paludis))
