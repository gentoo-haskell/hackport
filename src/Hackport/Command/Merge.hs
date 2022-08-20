module Hackport.Command.Merge
  ( mergeAction
  ) where

import Overlays
import Merge

import Hackport.Util (withHackportContext)
import Hackport.Env

mergeAction :: MonadEnv MergeEnv m => m ()
mergeAction = do
  (GlobalEnv verbosity op _, MergeEnv flags pkg) <- ask
  overlayPath <- liftIO $ getOverlayPath verbosity op
  withHackportContext $ \repoContext ->
    merge verbosity repoContext pkg overlayPath flags
