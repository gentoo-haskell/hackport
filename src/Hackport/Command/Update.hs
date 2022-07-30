module Hackport.Command.Update
  ( updateAction
  ) where

import Distribution.Simple.Command (CommandUI(..))
import qualified Distribution.Client.Setup as CabalInstall
import qualified Distribution.Client.Update as CabalInstall

import Hackport.Util (withHackportContext)
import Hackport.Env

updateAction :: Env env ()
updateAction  = askGlobalEnv >>= \(GlobalEnv verbosity _ _) ->
  withHackportContext $ \repoContext -> do
    let updateFlags = commandDefaultFlags CabalInstall.updateCommand
    liftIO $ CabalInstall.update verbosity updateFlags repoContext
