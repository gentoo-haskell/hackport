module Hackport.Command.Update
  ( CabalInstall.updateAction
  ) where

import Distribution.Simple.Command (CommandUI(..))
import qualified Distribution.Client.Setup as CabalInstall
import qualified Distribution.Client.CmdUpdate as CabalInstall

import Hackport.Util (withHackportContext)
import Hackport.Env

--updateAction :: Env env ()
--updateAction  = askGlobalEnv >>= \(GlobalEnv verbosity _ _) ->
--  withHackportContext $ \repoContext -> do
    --let updateFlags = commandDefaultFlags CabalInstall.updateCommand
    -- | TODO FIX THIS METHOD: https://github.com/Qeenon/cabal/blob/gentoo/cabal-install/src/Distribution/Client/CmdUpdate.hs
    --liftIO $ CabalInstall.updateRepo verbosity updateFlags repoContext reposToUpdate
