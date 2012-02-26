{-|
    Author      :  Sergei Trofimovich <slyfox@gentoo.org>
    Stability   :  experimental
    Portability :  haskell98

    Utilities to work with hackage-alike repositories
-}
module Hackage
    ( defaultRepo
    , defaultRepoURI
    ) where

import Distribution.Client.Types (Repo(..), RemoteRepo(..))
import Network.URI (URI(..), URIAuth(..))
import System.FilePath

defaultRepo :: FilePath -> Repo
defaultRepo overlayPath =
  Repo {
      repoKind = Left hackage,
      repoLocalDir = overlayPath </> ".hackport"
    }
  where
    hackage = RemoteRepo server_name uri
    server_name = "hackage.haskell.org"
    uri  = URI "http:" (Just (URIAuth "" server_name "")) "/packages/archive" "" ""

defaultRepoURI :: FilePath -> URI
defaultRepoURI overlayPath =
  case repoKind (defaultRepo overlayPath) of
    Left (RemoteRepo { remoteRepoURI = uri }) -> uri
    Right _                                   -> error $ "defaultRepoURI: unable to get URI for " ++ overlayPath
