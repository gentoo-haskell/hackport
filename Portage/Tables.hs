-- | Tables of portage specific convertations
module Portage.Tables
  ( set_build_slot
  ) where

import Portage.Dependency.Builder
import Portage.Dependency.Types
import Portage.PackageId

import Data.Monoid

set_build_slot :: Dependency -> Dependency
set_build_slot = 
  overAtom $ \a@(Atom pn dr (DAttr _ u)) -> 
      case mconcat $ map First $ map (matches a) slottedPkgs of
          First (Just s) -> Atom pn dr (DAttr s u)
          First Nothing  -> Atom pn dr (DAttr AnyBuildTimeSlot u)
    where
      matches (Atom pn _ _) (nm,s) 
        | pn == nm  = Just s
        | otherwise = Nothing

slottedPkgs :: [(PackageName, SlotDepend)]
slottedPkgs =
  [ (mkPackageName "dev-haskell" "quickcheck", GivenSlot "2=")
  , (mkPackageName "dev-haskell" "hdbc", GivenSlot "2=")
  ]
