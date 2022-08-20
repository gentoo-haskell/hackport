module HackPort.GlobalFlags
    ( GlobalFlags(..)
    , defaultGlobalFlags
    , withHackPortContext
    ) where

import qualified Distribution.Verbosity as DV
import qualified Distribution.Simple.Setup as DSS
import qualified Distribution.Client.Config as DCC
import qualified Distribution.Client.GlobalFlags as DCG
import qualified Distribution.Client.Types as DCT
import qualified Distribution.Utils.NubList as DUN

import qualified Network.URI as NU

import System.FilePath ((</>))

import qualified Overlays

-- | Type containing global flags.
data GlobalFlags =
    GlobalFlags { globalVersion :: DSS.Flag Bool
                , globalNumericVersion :: DSS.Flag Bool
                , globalPathToOverlay :: DSS.Flag (Maybe FilePath)
                , globalPathToPortage :: DSS.Flag (Maybe FilePath)
                }

-- | Default 'GlobalFlags'.
defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags =
    GlobalFlags { globalVersion = DSS.Flag False
                , globalNumericVersion = DSS.Flag False
                , globalPathToOverlay = DSS.Flag Nothing
                , globalPathToPortage = DSS.Flag Nothing
                }

-- | Default remote repository. Defaults to [hackage](hackage.haskell.org).
defaultRemoteRepo :: DCT.RemoteRepo
defaultRemoteRepo = DCC.addInfoForKnownRepos $ (DCT.emptyRemoteRepo (DCT.RepoName name)) { DCT.remoteRepoURI = uri }
   where
    Just uri = NU.parseURI "https://hackage.haskell.org/"
    name     = "hackage.haskell.org"

withHackPortContext :: DV.Verbosity -> GlobalFlags -> (DCG.RepoContext -> IO a) -> IO a
withHackPortContext verbosity global_flags callback = do
    overlayPath <- Overlays.getOverlayPath verbosity (DSS.fromFlag $ globalPathToOverlay global_flags)
    let flags = DCG.defaultGlobalFlags {
                    DCG.globalRemoteRepos = DUN.toNubList [defaultRemoteRepo]
                  , DCG.globalCacheDir    = DSS.Flag $ overlayPath </> ".hackport"
                }
    DCG.withRepoContext verbosity flags callback
