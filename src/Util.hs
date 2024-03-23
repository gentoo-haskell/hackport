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
    , warn
    , displayWarnings
    , die
    ) where

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import qualified Data.DList as DL
import System.IO
import System.Process
import System.Exit (ExitCode(..))
import qualified Distribution.Simple.Utils as Cabal
import qualified Distribution.Verbosity as Cabal
import Hackport.Env

-- | 'run_cmd' executes command and returns it's standard output
--   as 'String'.
run_cmd :: MonadIO m => String -> m (Maybe String)
run_cmd cmd = liftIO $ do
    (hI, hO, hE, hProcess) <- runInteractiveCommand cmd
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
debug s = withVerbosity $ \v -> liftIO $ Cabal.debug v s

notice :: (HasGlobalEnv m, MonadIO m) => String -> m ()
notice s = withVerbosity $ \v -> liftIO $ Cabal.notice v s

info :: (HasGlobalEnv m, MonadIO m) => String -> m ()
info s = withVerbosity $ \v -> liftIO $ Cabal.info v s

-- | Display a warning, then add a it to the global 'WarningBuffer', so that
--   it will be displayed at the end of hackport's output.
warn :: (HasGlobalEnv m, MonadIO m, MonadState WarningBuffer m) => String -> m ()
warn s = withVerbosity $ \v -> do
    liftIO $ Cabal.warn v s
    modifyWarningBuffer (<> DL.singleton s)

-- | Display all pending warnings, then terminate with an error message
die :: (MonadState WarningBuffer m, HasGlobalEnv m, MonadIO m) => String -> m a
die s = withVerbosity $ \v -> getWarningBuffer >>= \dl -> do
    displayWarnings v dl
    liftIO $ error s

withVerbosity :: (Monad m, HasGlobalEnv m) => (Cabal.Verbosity -> m a) -> m a
withVerbosity f = do
    verbosity <- globalVerbosity <$> askGlobalEnv
    f (Cabal.verboseNoWrap verbosity)
