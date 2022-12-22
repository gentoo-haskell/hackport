module Hackport.Command.Update
  ( updateAction
  ) where

import qualified Distribution.Client.CmdUpdate as CabalInstall

import Distribution.Client.Compat.Prelude (Verbosity)
import Distribution.Client.NixStyleOptions
  (NixStyleFlags(configFlags, projectFlags), defaultNixStyleFlags)
import Distribution.Client.ProjectFlags (ProjectFlags(flagIgnoreProject))
import Distribution.Client.Setup (ConfigFlags(configVerbosity), GlobalFlags(globalRemoteRepos))
import Distribution.Simple.Flag (Flag(Flag))
import Distribution.Utils.NubList (toNubList)

import Hackport.Util (withHackportContext)
import Hackport.Env

updateAction :: Env env ()
updateAction  = askGlobalEnv >>= \(GlobalEnv verbosity _ _) ->
  withHackportContext $ \globalFlags _repoContext -> do
    let nixFlags = ignoreProjectInNixFlags 
                        $ addVerbosityToNixFlags verbosity (defaultNixStyleFlags ())

    -- We need to unset the globalRemoteRepos set in 'withHackportContext' or
    -- we get a duplicate hackage repo. This creates a "file is locked" error.
    -- TODO: The cabal-install code needs to be examined further to see if
    -- there is a better way to do this (starting with Distribution.Client.CmdUpdate)
    let globalFlags' = globalFlags { globalRemoteRepos = toNubList [] }

    liftIO $ CabalInstall.updateAction nixFlags [] globalFlags'

-- | There is no verbosity argument for 'CabalInstall.updateAction'. It expects
--   the verbosity to be passed in via 'NixStyleFlags'.
addVerbosityToNixFlags :: Verbosity -> NixStyleFlags a -> NixStyleFlags a
addVerbosityToNixFlags v flags =
    let cFlags = configFlags flags
    in flags { configFlags = cFlags { configVerbosity = Flag v } }

-- | Without this it tries to treat the current directory as a cabal project,
--   complete with a @dist-newstyle@ directory.
ignoreProjectInNixFlags :: NixStyleFlags a -> NixStyleFlags a
ignoreProjectInNixFlags flags =
    let pFlags = projectFlags flags
    in flags { projectFlags = pFlags { flagIgnoreProject = Flag True } }
