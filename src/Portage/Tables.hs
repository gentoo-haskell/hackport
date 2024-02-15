{-|
Module      : Portage.Tables
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Tables of Portage-specific conversions.
-}
module Portage.Tables
  ( set_build_slot
  ) where

import Portage.Dependency.Builder
import Portage.Dependency.Types

-- | Set the @SLOT@ for a given 'Dependency'.
set_build_slot :: Dependency -> Dependency
set_build_slot =
  overAtom $ \(Atom pn dr (DAttr _ u)) ->
      Atom pn dr (DAttr AnyBuildTimeSlot u)
