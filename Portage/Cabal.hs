module Portage.Cabal
  (fromOverlay) where

import Data.List as List
import Data.Maybe
import qualified Data.Map as Map

import qualified Distribution.Package as Cabal
import qualified Distribution.Simple.PackageIndex as Cabal

import qualified Portage.Overlay as Portage
import qualified Portage.PackageId as Portage

fromOverlay :: Portage.Overlay -> Cabal.PackageIndex Portage.ExistingEbuild
fromOverlay overlay = Cabal.fromList $
  [ ebuild
  | (pn, ebuilds) <- Map.toAscList (Portage.overlayMap overlay)
  , ebuild <- ebuilds
  ]
