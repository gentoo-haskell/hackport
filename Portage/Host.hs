module Portage.Host
  ( getInfo -- :: IO [(String, String)]
  , LocalInfo(..)
  ) where

import Util (run_cmd)
import qualified Data.List.Split as DLS
import Data.Maybe (fromJust, isJust, catMaybes)
import Control.Applicative ( (<$>) )

import qualified System.Directory as D
import           System.FilePath ((</>))

import System.IO

data LocalInfo =
    LocalInfo { distfiles_dir :: String
              , overlay_list  :: [FilePath]
              , portage_dir   :: FilePath
              } deriving (Read, Show)

defaultInfo :: LocalInfo
defaultInfo = LocalInfo { distfiles_dir = "/usr/portage/distfiles"
                        , overlay_list  = []
                        , portage_dir   = "/usr/portage"
                        }

-- query paludis and then emerge
getInfo :: IO LocalInfo
getInfo = fromJust `fmap`
    performMaybes [ readConfig
                  , performMaybes [ getPaludisInfo
                                  , fmap parse_emerge_output <$> (run_cmd "emerge --info")
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
       case exists of
           True  -> read <$> readFile config_path
           False -> return Nothing

----------
-- Paludis
----------

getPaludisInfo :: IO (Maybe LocalInfo)
getPaludisInfo = fmap parsePaludisInfo <$> run_cmd "cave info"

parsePaludisInfo :: String -> LocalInfo
parsePaludisInfo text =
  let chunks = DLS.splitOn [""] . lines $ text
      repositories = catMaybes (map parseRepository chunks)
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

parse_emerge_output :: String -> LocalInfo
parse_emerge_output raw_data =
    foldl updateInfo defaultInfo $ lines raw_data
    where updateInfo info str =
              case (break (== '=') str) of
                  ("DISTDIR", '=':value)
                      -> info{distfiles_dir = unquote value}
                  ("PORTDIR", '=':value)
                      -> info{portage_dir = unquote value}
                  ("PORTDIR_OVERLAY", '=':value)
                      -> info{overlay_list = words $ unquote value}
                  _   -> info
          unquote = init . tail
