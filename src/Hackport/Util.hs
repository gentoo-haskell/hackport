module Hackport.Util
  ( getPortageDir
  , withHackportContext
  , defaultRemoteRepo
  ) where

import Data.Maybe (fromJust)
import qualified Network.URI as NU
import System.Directory ( doesDirectoryExist )
import System.FilePath ( (</>) )

import Distribution.Simple.Utils (warn)
import qualified Distribution.Simple.Setup as DSS
import qualified Distribution.Client.Config as DCC
import qualified Distribution.Client.GlobalFlags as DCG
import qualified Distribution.Client.Types as DCT
import qualified Distribution.Utils.NubList as DUN

import Portage.Host as Host ( getInfo, portage_dir )
import Overlays (getOverlayPath)

import Hackport.Env

getPortageDir :: MonadEnv env m => m FilePath
getPortageDir = do
  (GlobalEnv verbosity _ portagePathM, _) <- ask
  portagePath <- case portagePathM of
                   Nothing -> liftIO $ Host.portage_dir <$> Host.getInfo
                   Just path -> return path
  exists <- liftIO $ doesDirectoryExist $ portagePath </> "dev-haskell"
  unless exists $ liftIO $
    warn verbosity $ "Looks like an invalid portage directory: " ++ portagePath
  return portagePath

withHackportContext :: MonadEnv env m => (DCG.RepoContext -> IO a) -> m a
withHackportContext callback = do
    (GlobalEnv verbosity path _, _) <- ask
    overlayPath <- liftIO $ getOverlayPath verbosity path
    let flags = DCG.defaultGlobalFlags {
                    DCG.globalRemoteRepos = DUN.toNubList [defaultRemoteRepo]
                  , DCG.globalCacheDir    = DSS.Flag $ overlayPath </> ".hackport"
                }
    liftIO $ DCG.withRepoContext verbosity flags callback

-- | Default remote repository. Defaults to [hackage](hackage.haskell.org).
defaultRemoteRepo :: DCT.RemoteRepo
defaultRemoteRepo = DCC.addInfoForKnownRepos
    (DCT.emptyRemoteRepo (DCT.RepoName name)) { DCT.remoteRepoURI = uri }
   where
    uri  = fromJust $ NU.parseURI "https://hackage.haskell.org/"
    name = "hackage.haskell.org"
