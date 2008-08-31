module Bash where

import Control.Monad.Error
import System.Process
import System.Directory
import System.IO
import System.Exit

import Error

getSystemPortdir :: IO String
getSystemPortdir = do
  dir <- runBash "source /etc/make.conf;echo -n $PORTDIR"
  case dir of
    "" -> return "/usr/portage"
    _ -> return dir

runBash :: String -- ^ The command line
	-> IO String -- ^ The command-line's output
runBash command = do
  mpath <- findExecutable "bash"
  bash <- maybe (throwEx BashNotFound) return mpath
  (inp,outp,err,pid) <- runInteractiveProcess bash ["-c",command] Nothing Nothing
  hClose inp
  result <- hGetContents outp
  errors <- hGetContents err
  length result `seq` hClose outp
  length errors `seq` hClose err
  exitCode <- waitForProcess pid
  case exitCode of
    ExitFailure _ -> throwEx (BashError errors)
    ExitSuccess   -> return result
