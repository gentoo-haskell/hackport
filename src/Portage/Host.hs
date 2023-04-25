module Portage.Host
  ( getInfo -- :: IO [(String, String)]
  , LocalInfo(..)
  ) where

import Util (run_cmd)
import qualified Data.List.Split as DLS
import Data.Maybe (fromJust, isJust, mapMaybe)

import qualified System.Directory as D
import           System.FilePath ((</>))

import System.IO

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
getInfo :: IO LocalInfo
getInfo = fromJust `fmap`
    performMaybes [ readConfig
                  , performMaybes [ getPaludisInfo
                                  , askPortageq
                                  , return (Just defaultInfo)
                                  ] >>= showAnnoyingWarning
                  ]
    where performMaybes [] = return Nothing
          performMaybes (act:acts) =
              do r <- act
                 if isJust r
                     then return r
                     else performMaybes acts

showAnnoyingWarning :: Maybe LocalInfo -> IO (Maybe LocalInfo)
showAnnoyingWarning info = do
    hPutStr stderr $ unlines [ "-- Consider creating ~/" ++ hackport_config ++ " file with contents:"
                             , show info
                             , "-- It will speed hackport startup time a bit."
                             ]
    return info

-- relative to home dir
hackport_config :: FilePath
hackport_config = ".hackport" </> "repositories"

--------------------------
-- fastest: config reading
--------------------------
readConfig :: IO (Maybe LocalInfo)
readConfig =
    do home_dir <- D.getHomeDirectory
       let config_path  = home_dir </> hackport_config
       exists <- D.doesFileExist config_path
       if exists then read <$> readFile config_path else return Nothing

----------
-- Paludis
----------

getPaludisInfo :: IO (Maybe LocalInfo)
getPaludisInfo = fmap parsePaludisInfo <$> run_cmd "cave info"

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
    let dict = [ (head ln, unwords (tail ln)) | ln <- map words lns ]
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

askPortageq :: IO (Maybe LocalInfo)
askPortageq = do
    distdir <- run_cmd "portageq distdir"
    portdir <- run_cmd "portageq get_repo_path / gentoo"
    hsRepo  <- run_cmd "portageq get_repo_path / haskell"
    --There really ought to be both distdir and portdir,
    --but maybe no hsRepo defined yet.
    let info = if Nothing `elem` [distdir,portdir]
               then Nothing
               else Just LocalInfo
                      { distfiles_dir = grab distdir
                      , portage_dir = grab portdir
                      , overlay_list = iffy hsRepo
                      }
                 --init: kill newline char
                 where grab = init . fromJust
                       iffy Nothing     = []
                       iffy (Just repo) = [init repo]
    return info
