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
      repoKind = Left defaultRemoteRepo,
      repoLocalDir = overlayPath </> ".hackport"
    }

-- A copy from cabal-install/Distribution.Client.Config
defaultRemoteRepo :: RemoteRepo
defaultRemoteRepo = RemoteRepo name uri () False
  where
    name = "hackage.haskell.org"
    uri  = URI "http:" (Just (URIAuth "" name "")) "/" "" ""

defaultRepoURI :: FilePath -> URI
defaultRepoURI overlayPath =
  case repoKind (defaultRepo overlayPath) of
    Left (RemoteRepo { remoteRepoURI = uri }) -> uri
    Right _                                   -> error $ "defaultRepoURI: unable to get URI for " ++ overlayPath
