{- | Basic helpers to build depend structures -}
module Portage.Dependency.Builder
  (
    empty_dependency
  , addDepUseFlag
  , setSlotDep
  ) where

import Portage.Dependency.Types
import Portage.Use

empty_dependency :: Dependency
empty_dependency = DependAllOf []

addDepUseFlag :: UseFlag -> Dependency -> Dependency
addDepUseFlag n (DependAllOf d) = DependAllOf $ map (addDepUseFlag n) d
addDepUseFlag n (Atom pn dr (DAttr s u)) = Atom pn dr (DAttr s (n:u))
addDepUseFlag n (DependAnyOf d) = DependAnyOf $ map (addDepUseFlag n) d
addDepUseFlag n (DependIfUse u d) = DependIfUse u (addDepUseFlag n d)

setSlotDep :: SlotDepend -> Dependency -> Dependency
setSlotDep n (DependAllOf d) = DependAllOf $ map (setSlotDep n) d
setSlotDep n (Atom pn dr (DAttr _s u)) = Atom pn dr (DAttr n u)
setSlotDep n (DependAnyOf d) = DependAnyOf $ map (setSlotDep n) d
setSlotDep n (DependIfUse u d) = DependIfUse u (setSlotDep n d)
