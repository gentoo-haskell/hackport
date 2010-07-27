module Portage.Host
  ( getInfo -- :: IO [(String, String)]
  , LocalInfo(..)
  ) where

import Util (run_cmd)
import Data.Char (isSpace)
import Data.Maybe (fromJust, isJust)
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
                  , (fmap . fmap) parse_emerge_output  (run_cmd "emerge --info")
                  , return (Just defaultInfo)
                  ]
    where performMaybes [] = return Nothing
          performMaybes (act:acts) =
              do r <- act
                 if isJust r
                     then return r
                     else performMaybes acts

data LocalPaludisOverlay =
    LocalPaludisOverlay { repo_name :: String
                        , format    :: String
                        , location  :: FilePath
                        , distdir   :: FilePath
                        }

bad_paludis_overlay :: LocalPaludisOverlay
bad_paludis_overlay =
    LocalPaludisOverlay { repo_name = undefined
                        , format    = undefined
                        , location  = undefined
                        , distdir   = undefined
                        }

getPaludisInfo :: IO (Maybe LocalInfo)
getPaludisInfo = do
  mp <- fmap paludisParseRepositories <$> run_cmd "paludis --list-repositories"
  case mp of
    Nothing -> return Nothing
    Just repos0 -> Just <$> do
      let repos = filter (`notElem` knownRepos) repos0
      Just gentooLocation <- paludisRepositoryLocation "gentoo"
      Just gentooDistdir <- paludisRepositoryDistdir "gentoo"
      others <- map fromJust <$> mapM paludisRepositoryLocation repos
      return (LocalInfo
                { distfiles_dir = gentooDistdir
                , overlay_list = others
                , portage_dir = gentooLocation
                })
  where
    knownRepos = ["installed-virtuals", "virtuals", "gentoo", "installed"]

paludisParseRepositories :: String -> [String]
paludisParseRepositories = map (tail . tail) . lines
{- * installed-virtuals
   * virtuals
   * gentoo
   * installed
   * gentoo-haskell -}

paludisRepositoryLocation :: String -> IO (Maybe String)
paludisRepositoryLocation repo = fmap init <$> run_cmd ("paludis --configuration-variable " ++ repo ++ " location")

paludisRepositoryDistdir :: String -> IO (Maybe String)
paludisRepositoryDistdir repo = fmap init <$> run_cmd ("paludis --configuration-variable " ++ repo ++ " distdir")

parse_paludis_output :: String -> LocalInfo
parse_paludis_output raw_data =
    foldl updateInfo defaultInfo $ parse_paludis_overlays raw_data
    where updateInfo info po =
              case (format po) of
                  "ebuild" | (repo_name po) /= "gentoo" -- hack, skip main repo
                      -> info{ distfiles_dir = distdir po -- we override last distdir here (FIXME?)
                             , overlay_list  = (location po) : overlay_list info
                             }
                  "ebuild" -- hack, main repo -- (repo_name po) == "gentoo"
                      -> info{ portage_dir = location po }

                  _   -> info

parse_paludis_overlays :: String -> [LocalPaludisOverlay]
parse_paludis_overlays raw_data =
    parse_paludis_overlays' (reverse $ lines raw_data) bad_paludis_overlay

-- parse in reverse order :]
parse_paludis_overlays' :: [String] -> LocalPaludisOverlay -> [LocalPaludisOverlay]
parse_paludis_overlays' [] _ = []
parse_paludis_overlays' (l:ls) info =
    case (words l) of
    -- look for "Repository <repo-name>:"
    ["Repository", r_name] -> info{repo_name = init r_name} :
                                      go bad_paludis_overlay
    -- else - parse attributes
    _ -> case (break (== ':') (refine l)) of
            ("location", ':':value)
                -> go info{location = refine value}
            ("distdir", ':':value)
                -> go info{distdir = refine value}
            ("format", ':':value)
                -> go info{format = refine value}
            _   -> go info
    where go = parse_paludis_overlays' ls
          refine = dropWhile isSpace

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
