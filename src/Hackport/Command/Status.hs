module Hackport.Command.Status
  ( statusAction
  ) where

import Overlays
import Status

import Hackport.Env
import Hackport.Util

statusAction :: MonadEnv StatusEnv m => m ()
statusAction = do
  (GlobalEnv verbosity po _, StatusEnv direction pkgs) <- ask
  portagePath <- getPortageDir
  overlayPath <- liftIO $ getOverlayPath verbosity po

  withHackportContext $ \repoContext ->
      runStatus verbosity portagePath overlayPath direction pkgs repoContext
