{- | Basic helpers to build depend structures -}
module Portage.Dependency.Builder
  (
    empty_dependency
  , addDepUseFlag
  , setSlotDep
  , mkUseDependency
  , overAtom
  ) where

import Portage.Dependency.Types
import Portage.Use

-- TODO: remove it and switch to 'SatisfiedDepend' instead
empty_dependency :: Dependency
empty_dependency = DependAllOf []

addDepUseFlag :: UseFlag -> Dependency -> Dependency
addDepUseFlag n = overAtom (\(Atom pn dr (DAttr s u)) -> Atom pn dr (DAttr s (n:u)))

setSlotDep :: SlotDepend -> Dependency -> Dependency
setSlotDep n = overAtom (\(Atom pn dr (DAttr _s u)) -> Atom pn dr (DAttr n u))

mkUseDependency :: (Bool, Use) -> Dependency -> Dependency
mkUseDependency (b, u) d =
    case b of
        True  -> DependIfUse u d empty_dependency
        False -> DependIfUse u empty_dependency d

overAtom :: (Atom -> Atom) -> Dependency -> Dependency
overAtom f (DependAllOf d) = DependAllOf $ map (overAtom f) d
overAtom f (DependAnyOf d) = DependAnyOf $ map (overAtom f) d
overAtom f (DependIfUse u d1 d2) = DependIfUse u (f `overAtom` d1) (f `overAtom` d2)
overAtom f (DependAtom a) = DependAtom (f a)
