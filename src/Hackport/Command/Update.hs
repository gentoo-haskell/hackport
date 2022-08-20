module Hackport.Command.Update
  ( updateAction
  ) where

import Distribution.Simple.Command (CommandUI(..))
import qualified Distribution.Client.Setup as CabalInstall
import qualified Distribution.Client.Update as CabalInstall

import Hackport.Util (withHackportContext)
import Hackport.Env

updateAction :: MonadEnv env m => m ()
updateAction  = do
  (GlobalEnv verbosity _ _, _) <- ask

  withHackportContext $ \repoContext -> do
    let updateFlags = commandDefaultFlags CabalInstall.updateCommand
    CabalInstall.update verbosity updateFlags repoContext
