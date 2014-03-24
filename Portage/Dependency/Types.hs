module Portage.Dependency.Types
  (
    SlotDepend(..)
  , LBound(..)
  , UBound(..)
  , DRange(..)
  , DAttr(..)
  , Dependency(..)
  , dep_is_case_of
  ) where

import Portage.PackageId
import Portage.Use

data SlotDepend = AnySlot          -- nothing special
                | AnyBuildTimeSlot -- ':='
                | GivenSlot String -- ':slotno'
    deriving (Eq, Show)

data LBound = StrictLB    Version
            | NonstrictLB Version
            | ZeroB
    deriving (Eq, Show)

instance Ord LBound where
    compare ZeroB ZeroB = EQ
    compare ZeroB _     = LT
    compare _     ZeroB = GT
    compare (StrictLB lv)    (StrictLB rv)    = compare lv rv
    compare (NonstrictLB lv) (NonstrictLB rv) = compare lv rv
    compare l r = error $ unlines ["i am too lazy to implement LBound: compare"
                                  , show l
                                  , show r]

data UBound = StrictUB Version   -- <
            | NonstrictUB Version -- <=
            | InfinityB
    deriving (Eq, Show)

instance Ord UBound where
    compare InfinityB InfinityB = EQ
    compare InfinityB _     = GT
    compare _         InfinityB = LT
    compare (NonstrictUB lv) (NonstrictUB rv) = compare lv rv
    compare (StrictUB lv)    (StrictUB rv)    = compare lv rv
    compare l r = error $ unlines ["i am too lazy to implement UBound: compare"
                                  , show l
                                  , show r]

data DRange = DRange LBound UBound
            | DExact Version
    deriving (Eq, Show)

-- True if 'left' "interval" is a nonstrict subset of 'right' "interval"
range_is_case_of :: DRange -> DRange -> Bool
range_is_case_of (DRange llow lup) (DRange rlow rup)
    | llow >= rlow && lup <= rup = True
range_is_case_of _ _ = False

data DAttr = DAttr SlotDepend [UseFlag]
    deriving (Eq, Show)

data Dependency = Atom PackageName DRange DAttr
                | DependIfUse Use      Dependency Dependency -- u? ( td ) !u? ( fd )
                | DependAnyOf         [Dependency]
                | DependAllOf         [Dependency]
    deriving (Eq, Show)

dep_is_case_of :: Dependency -> Dependency -> Bool
dep_is_case_of l r
    -- very broad (not only on atoms) special case
    | l == r = True
-- only on atoms
dep_is_case_of (Atom lpn lr lda) (Atom rpn rr rda)
    | lpn == rpn && lda == rda = lr `range_is_case_of` rr
dep_is_case_of _ _ = False
