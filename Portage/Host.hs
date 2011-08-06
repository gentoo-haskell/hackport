module Portage.Host
  ( getInfo -- :: IO [(String, String)]
  , LocalInfo(..)
  ) where

import Util (run_cmd)
import Data.Maybe (fromJust, isJust, catMaybes)
import Control.Applicative ( (<$>) )

data LocalInfo =
    LocalInfo { distfiles_dir :: String
              , overlay_list  :: [FilePath]
              , portage_dir   :: FilePath
              } deriving Show

defaultInfo :: LocalInfo
defaultInfo = LocalInfo { distfiles_dir = "/usr/portage/distfiles"
                        , overlay_list  = []
                        , portage_dir   = "/usr/portage"
                        }

-- query paludis and then emerge
getInfo :: IO LocalInfo
getInfo = fromJust `fmap`
    performMaybes [ getPaludisInfo
                  , fmap parse_emerge_output <$> (run_cmd "emerge --info")
                  , return (Just defaultInfo)
                  ]
    where performMaybes [] = return Nothing
          performMaybes (act:acts) =
              do r <- act
                 if isJust r
                     then return r
                     else performMaybes acts

----------
-- Paludis
----------

getPaludisInfo :: IO (Maybe LocalInfo)
getPaludisInfo = fmap parsePaludisInfo <$> run_cmd "paludis --info"

parsePaludisInfo :: String -> LocalInfo
parsePaludisInfo text =
  let chunks = splitBy (=="") . lines $ text
      repositories = catMaybes (map parseRepository chunks)
  in  fromJust (mkLocalInfo repositories)
  where
  parseRepository :: [String] -> Maybe (String, (String, String))
  parseRepository (firstLine:lns) = do
    name <- case words firstLine of
                ["Repository", nm] -> return (init nm)
                _ -> fail "not a repository chunk"
    let dict = [ (head ln, unwords (tail ln)) | ln <- map words lns ]
    location <- lookup "location:" dict
    distfiles <- lookup "distdir:" dict
    return (name, (location, distfiles))

  knownRepos = ["installed-virtuals", "virtuals", "gentoo", "installed"]

  mkLocalInfo :: [(String, (String, String))] -> Maybe LocalInfo
  mkLocalInfo repos = do
    (gentooLocation, gentooDistfiles) <- lookup "gentoo" repos
    let overlays = [ loc | (name, (loc, _dist)) <- repos, name `notElem` knownRepos ]
    return (LocalInfo
              { distfiles_dir = gentooDistfiles
              , portage_dir = gentooLocation
              , overlay_list = overlays
              })

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy c [] = []
splitBy c lst =
  let (x,xs) = break c lst
      (_,xs') = span c xs
  in x : splitBy c xs'

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
