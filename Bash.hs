module Bash where

import Control.Monad.Error
import System.Process
import System.Directory
import System.IO
import System.Exit

import Action
import Config
import Error
import CacheFile

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

getOverlays :: HPAction [String]
getOverlays = runBash "source /etc/make.conf;echo -n $PORTDIR_OVERLAY" >>= (return.words)

getSystemPortdir :: HPAction String
getSystemPortdir = do
    dir <- runBash "source /etc/make.conf;echo -n $PORTDIR"
    case dir of
        "" -> return "/usr/portage"
        _ -> return dir

getPortdir :: HPAction String
getPortdir = do
    cfg <- getCfg
    case portagePath cfg of
        Just dir -> return dir
        Nothing -> do
            sys <- getSystemPortdir
            setPortagePath (Just sys)
            return sys

runBash ::
	String -> -- ^ The command line
	HPAction String -- ^ The command-line's output
runBash command = do
	mpath <- liftIO $ findExecutable "bash"
	bash <- maybe (throwError BashNotFound) return mpath
	(inp,outp,err,pid) <- liftIO $ runInteractiveProcess bash ["-c",command] Nothing Nothing
	liftIO $ hClose inp
	result <- liftIO $ hGetContents outp
	errors <- liftIO $ hGetContents err
	length result `seq` liftIO (hClose outp)
	length errors `seq` liftIO (hClose err)
	exitCode <- liftIO $ waitForProcess pid
	case exitCode of
		ExitFailure _ -> throwError $ BashError errors
		ExitSuccess   -> return result
