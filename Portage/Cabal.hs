module Portage.Cabal
  (fromOverlay) where

import qualified Data.Map as Map

import qualified Distribution.Client.PackageIndex as Cabal

import qualified Portage.Overlay as Portage

fromOverlay :: Portage.Overlay -> Cabal.PackageIndex Portage.ExistingEbuild
fromOverlay overlay = Cabal.fromList $
  [ ebuild
  | (_pn, ebuilds) <- Map.toAscList (Portage.overlayMap overlay)
  , ebuild <- ebuilds
  ]
