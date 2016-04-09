{-|
    Author      :  Sergei Trofimovich <slyfox@gentoo.org>
    Stability   :  experimental
    Portability :  haskell98

    Utilities to work with hackage-alike repositories
-}
module Hackage ( defaultRemoteRepo ) where

import Distribution.Client.Types as DCT
import qualified Network.URI as NU

defaultRemoteRepo :: DCT.RemoteRepo
defaultRemoteRepo = (DCT.emptyRemoteRepo name) { DCT.remoteRepoURI = uri }
   where
    Just uri = NU.parseURI "https://hackage.haskell.org/"
    name     = "hackage.haskell.org"
