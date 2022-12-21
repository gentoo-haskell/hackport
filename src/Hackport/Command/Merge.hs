module Hackport.Command.Merge
  ( mergeAction
  ) where

import Merge (merge)

import Hackport.Util (withHackportContext)
import Hackport.Env

mergeAction :: Env MergeEnv ()
mergeAction = askEnv >>= \(MergeEnv flags pkg) -> do
  withHackportContext $ \_ repoContext ->
    merge repoContext pkg flags
