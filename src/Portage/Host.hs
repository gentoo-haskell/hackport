module Portage.Host
  ( getInfo -- :: IO [(String, String)]
  , LocalInfo(..)
  ) where

import Util (run_cmd)
import qualified Data.List.Split as DLS
import Data.Maybe (fromJust, isJust, mapMaybe, fromMaybe)

import qualified System.Directory as D
import           System.FilePath ((</>))
import           Hackport.Dirs (hackportDir)

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Hackport.Env (HasGlobalEnv, WarningBuffer)
import Util (warn)

data LocalInfo =
    LocalInfo { distfiles_dir :: String
              , overlay_list  :: [FilePath]
              , portage_dir   :: FilePath
              } deriving (Read, Show)

defaultInfo :: LocalInfo
defaultInfo = LocalInfo { distfiles_dir = "/var/cache/distfiles"
                        , overlay_list  = []
                        , portage_dir   = "/var/db/repos/gentoo"
                        }

-- query paludis and then emerge
getInfo :: (HasGlobalEnv m, MonadIO m, MonadState WarningBuffer m)
    => m LocalInfo
getInfo = fromMaybe defaultInfo <$> runMaybeT (readConfig <|> getInfoWithWarning)
  where
    getInfoWithWarning = MaybeT $ do
        configPath <- hackportConfig
        info <- runMaybeT $ getPaludisInfo <|> askPortageq
        warn $ configWarning configPath info
        pure info

    configWarning configPath info = unlines
        [ "-- Consider creating " ++ configPath ++ " file with contents:"
        , show info
        , "-- It will speed hackport startup time a bit."
        ]

-- | Where @repositories@ config file is located
hackportConfig :: MonadIO m => m FilePath
hackportConfig = (</> "repositories") <$> hackportDir

--------------------------
-- fastest: config reading
--------------------------
readConfig :: MonadIO m => MaybeT m LocalInfo
readConfig = do
    configPath <- hackportConfig
    liftIO (D.doesFileExist configPath) >>= guard
    MaybeT $ liftIO $ read <$> readFile configPath

----------
-- Paludis
----------

getPaludisInfo :: MonadIO m => MaybeT m LocalInfo
getPaludisInfo = MaybeT $ fmap parsePaludisInfo <$> run_cmd "cave info"

parsePaludisInfo :: String -> LocalInfo
parsePaludisInfo text =
  let chunks = DLS.splitOn [""] . lines $ text
      repositories = mapMaybe parseRepository chunks
  in  fromJust (mkLocalInfo repositories)
  where
  parseRepository :: [String] -> Maybe (String, (String, String))
  parseRepository [] = Nothing
  parseRepository (firstLine:lns) = do
    name <- case words firstLine of
                ["Repository", nm] -> return (init nm)
                _ -> fail "not a repository chunk"
    let dict = [ (lnHead, unwords lnTail) | (lnHead:lnTail) <- map words lns ]
    location <- lookup "location" dict
    distfiles <- lookup "distdir" dict
    return (name, (location, distfiles))

  mkLocalInfo :: [(String, (String, String))] -> Maybe LocalInfo
  mkLocalInfo repos = do
    (gentooLocation, gentooDistfiles) <- lookup "gentoo" repos
    let overlays = [ loc | (_, (loc, _dist)) <- repos ]
    return (LocalInfo
              { distfiles_dir = gentooDistfiles
              , portage_dir = gentooLocation
              , overlay_list = overlays
              })

---------
-- Emerge
---------

askPortageq :: MonadIO m => MaybeT m LocalInfo
askPortageq = do
    distdir <- run_cmd "portageq distdir"
    portdir <- run_cmd "portageq get_repo_path / gentoo"
    hsRepo  <- run_cmd "portageq get_repo_path / haskell"
    --There really ought to be both distdir and portdir,
    --but maybe no hsRepo defined yet.
    guard $ all isJust [distdir,portdir]
    pure LocalInfo
        { distfiles_dir = grab distdir
        , portage_dir = grab portdir
        , overlay_list = iffy hsRepo
        }
  where
    --init: kill newline char
    grab = init . fromJust
    iffy Nothing     = []
    iffy (Just repo) = [init repo]
