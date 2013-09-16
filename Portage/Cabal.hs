module Portage.Cabal
  ( fromOverlay
  , convertLicense
  ) where

import qualified Data.Map as Map

import qualified Distribution.Client.PackageIndex as Cabal
import qualified Distribution.License             as Cabal
import qualified Distribution.Text                as Cabal

import qualified Portage.Overlay as Portage

fromOverlay :: Portage.Overlay -> Cabal.PackageIndex Portage.ExistingEbuild
fromOverlay overlay = Cabal.fromList $
  [ ebuild
  | (_pn, ebuilds) <- Map.toAscList (Portage.overlayMap overlay)
  , ebuild <- ebuilds
  ]

-- map the cabal license type to the gentoo license string format
convertLicense :: Cabal.License -> Either String String
convertLicense l =
    case l of
        --  good ones
        Cabal.GPL mv       -> Right $ "GPL-" ++ (maybe "2" Cabal.display mv)  -- almost certainly version 2
        Cabal.LGPL mv      -> Right $ "LGPL-" ++ (maybe "2.1" Cabal.display mv) -- probably version 2.1
        Cabal.BSD3         -> Right "BSD"
        Cabal.BSD4         -> Right "BSD-4"
        Cabal.PublicDomain -> Right "public-domain"
        Cabal.MIT          -> Right "MIT"
        Cabal.Apache mv    -> Right $ "Apache-" ++ (maybe "1.1" Cabal.display mv) -- probably version 1.1
        -- bad ones
        Cabal.AllRightsReserved -> Left "EULA-style licence. Please pick it manually."
        Cabal.UnknownLicense _  -> Left "license unknown to cabal. Please pick it manually."
        Cabal.OtherLicense      -> Left "Please look at license file of package and pick it manually."
