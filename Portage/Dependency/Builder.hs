{- | Basic helpers to build depend structures -}
module Portage.Dependency.Builder
  (
    empty_dependency
  , addDepUseFlag
  , setSlotDep
  , mkUseDependency
  ) where

import Portage.Dependency.Types
import Portage.Use

-- TODO: remove it and switch to 'SatisfiedDepend' instead
empty_dependency :: Dependency
empty_dependency = DependAllOf []

addDepUseFlag :: UseFlag -> Dependency -> Dependency
addDepUseFlag n (DependAllOf d) = DependAllOf $ map (addDepUseFlag n) d
addDepUseFlag n (Atom pn dr (DAttr s u)) = Atom pn dr (DAttr s (n:u))
addDepUseFlag n (DependAnyOf d) = DependAnyOf $ map (addDepUseFlag n) d
addDepUseFlag n (DependIfUse u td fd) = DependIfUse u (addDepUseFlag n td) (addDepUseFlag n fd)

setSlotDep :: SlotDepend -> Dependency -> Dependency
setSlotDep n (DependAllOf d) = DependAllOf $ map (setSlotDep n) d
setSlotDep n (Atom pn dr (DAttr _s u)) = Atom pn dr (DAttr n u)
setSlotDep n (DependAnyOf d) = DependAnyOf $ map (setSlotDep n) d
setSlotDep n (DependIfUse u td fd) = DependIfUse u (setSlotDep n td) (setSlotDep n fd)

mkUseDependency :: (Bool, Use) -> Dependency -> Dependency
mkUseDependency (b, u) d =
    case b of
        True  -> DependIfUse u d empty_dependency
        False -> DependIfUse u empty_dependency d
