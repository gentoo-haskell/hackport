module Overlays where

import Control.Monad.Error
import System.Directory
import Data.Maybe
import Data.List (nub)

import Bash
import Action
import Config
import Error
import CacheFile

getOverlayPath :: HPAction String
getOverlayPath = do
    cfg <- getCfg
    case overlayPath cfg of
        Nothing -> do
          tree <- getOverlay `sayDebug` ("Guessing overlay...\n",\tree->"Found '"++tree++"'")
          setOverlayPath $ Just tree
          return tree
        Just tree -> return tree

getOverlay :: HPAction String
getOverlay = do
	overlays <- getOverlays
	case overlays of
		[] -> throwError NoOverlay
		[x] -> return x
		mul -> search mul
  where
  search :: [String] -> HPAction String
  search mul = do
    let loop [] = throwError $ MultipleOverlays mul
        loop (x:xs) = (do
          found <- liftIO (doesFileExist (cacheFile x))
		`sayDebug` ("Checking '"++x++"'...\n",\res->if res then "found.\n" else "not found.")
          if found
            then return x
            else loop xs)
    whisper "There are several overlays in your /etc/make.conf"
    mapM (\x-> whisper (" * " ++x)) mul
    whisper "Looking for one with a HackPort cache..."
    overlay <- loop mul
    whisper ("I choose " ++ overlay)
    whisper "Override my decision with hackport -p /my/overlay"
    return overlay

portageOverlays :: HPAction [String]
portageOverlays = runBash "source /etc/make.conf;echo -n $PORTDIR_OVERLAY" >>= (return.words)

paludisOverlays :: HPAction [String]
paludisOverlays = return []

getOverlays :: HPAction [String]
getOverlays = do
    portage <- portageOverlays
    paludis <- paludisOverlays
    return (nub (portage ++ paludis))
