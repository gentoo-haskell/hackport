{-|
    Author      :  Sergei Trofimovich <slyfox@inbox.ru>
    Stability   :  experimental
    Portability :  haskell98

    Ungrouped utilitary stuff lays here until someone finds better place for it :]
-}

module Util
    ( run_cmd -- :: String -> IO (Maybe String)
    , debug
    , notice
    , info
    ) where

import System.IO
import System.Process
import System.Exit (ExitCode(..))
import qualified Distribution.Simple.Utils as Cabal
import Hackport.Env

-- 'run_cmd' executes command and returns it's standard output
-- as 'String'.

run_cmd :: String -> IO (Maybe String)
run_cmd cmd = do (hI, hO, hE, hProcess) <- runInteractiveCommand cmd
                 hClose hI
                 output <- hGetContents hO
                 errors <- hGetContents hE -- TODO: propagate error to caller
                 length output `seq` hClose hO
                 length errors `seq` hClose hE

                 exitCode <- waitForProcess hProcess
                 return $ if (output == "" || exitCode /= ExitSuccess)
                          then Nothing
                          else Just output

debug :: (HasGlobalEnv m, MonadIO m) => String -> m ()
debug s = askGlobalEnv >>= \(GlobalEnv v _ _) -> liftIO $ Cabal.debug v s

notice :: (HasGlobalEnv m, MonadIO m) => String -> m ()
notice s = askGlobalEnv >>= \(GlobalEnv v _ _) -> liftIO $ Cabal.notice v s

info :: (HasGlobalEnv m, MonadIO m) => String -> m ()
info s = askGlobalEnv >>= \(GlobalEnv v _ _) -> liftIO $ Cabal.info v s
