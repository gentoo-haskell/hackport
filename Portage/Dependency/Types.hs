module Portage.Dependency.Types
  (
    SlotDepend(..)
  , LBound(..)
  , UBound(..)
  , DRange(..)
  , DAttr(..)
  , Dependency(..)
  , dep_as_broad_as
  , is_empty_dependency
  ) where

import Portage.PackageId
import Portage.Use

data SlotDepend = AnySlot          -- nothing special
                | AnyBuildTimeSlot -- ':='
                | GivenSlot String -- ':slotno'
    deriving (Eq, Show, Ord)

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
    compare (StrictLB lv)    (NonstrictLB rv) = case compare lv rv of
                                                    EQ -> GT
                                                    r  -> r
    compare (NonstrictLB lv) (StrictLB rv)    = case compare lv rv of
                                                    EQ -> LT
                                                    r  -> r

data UBound = StrictUB Version   -- <
            | NonstrictUB Version -- <=
            | InfinityB
    deriving (Eq, Show)

instance Ord UBound where
    compare InfinityB InfinityB = EQ
    compare InfinityB _     = GT
    compare _         InfinityB = LT
    compare (StrictUB lv)    (StrictUB rv)    = compare lv rv
    compare (NonstrictUB lv) (NonstrictUB rv) = compare lv rv
    compare (StrictUB lv)    (NonstrictUB rv) = case compare lv rv of
                                                    EQ -> LT
                                                    r  -> r
    compare (NonstrictUB lv) (StrictUB rv)    = case compare lv rv of
                                                    EQ -> GT
                                                    r  -> r

data DRange = DRange LBound UBound
            | DExact Version
    deriving (Eq, Show, Ord)

-- True if 'left' "interval" covers at least as much as the 'right' "interval"
range_as_broad_as :: DRange -> DRange -> Bool
range_as_broad_as (DRange llow lup) (DRange rlow rup)
    | llow <= rlow && lup >= rup = True
range_as_broad_as _ _ = False

data DAttr = DAttr SlotDepend [UseFlag]
    deriving (Eq, Show, Ord)

data Dependency = Atom PackageName DRange DAttr
                | DependIfUse Use      Dependency Dependency -- u? ( td ) !u? ( fd )
                | DependAnyOf         [Dependency]
                | DependAllOf         [Dependency]
    deriving (Eq, Show, Ord)

-- returns 'True' if left constraint is the same (or looser) than right
dep_as_broad_as :: Dependency -> Dependency -> Bool
dep_as_broad_as l r
    -- very broad (not only on atoms) special case
    | l == r = True
-- atoms
dep_as_broad_as (Atom lpn lr lda) (Atom rpn rr rda)
    | lpn == rpn && lda == rda = lr `range_as_broad_as` rr
-- AllOf (very common case in context propagation)
dep_as_broad_as d (DependAllOf deps)
    | any (dep_as_broad_as d) deps = True
dep_as_broad_as _ _ = False

-- TODO: remove it and switch to 'SatisfiedDepend' instead
is_empty_dependency :: Dependency -> Bool
is_empty_dependency d =
    case d of
        DependIfUse _use td fd
            -> is_empty_dependency td && is_empty_dependency fd
        DependAnyOf []
            -> True -- 'any (const True) [] == False' and we don't want it
        DependAnyOf deps
            -> any is_empty_dependency deps
        DependAllOf deps
            -> all is_empty_dependency deps
        Atom _pn _dr _dattr
            -> False
