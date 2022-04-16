{-|
Module      : Portage.Dependency.Types
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Functions and types related to processing Portage dependencies.
-}
module Portage.Dependency.Types
  (
    SlotDepend(..)
  , LBound(..)
  , UBound(..)
  , DRange(..)
  , DAttr(..)
  , Dependency(..)
  , Atom(..)
  , dep_as_broad_as
  , is_empty_dependency
  , dep_is_case_of
  , range_is_case_of
  ) where

import           Portage.PackageId
import           Portage.Use

import           Control.DeepSeq (NFData(..))

-- | Type of SLOT dependency of a dependency.
data SlotDepend = AnySlot          -- ^ nothing special
                | AnyBuildTimeSlot -- ^ ':='
                | GivenSlot String -- ^ ':slotno'
    deriving (Eq, Show, Ord)

instance NFData SlotDepend where
  rnf AnySlot = ()
  rnf AnyBuildTimeSlot = ()
  rnf (GivenSlot s) = rnf s

-- | Type of lower bound of a dependency.
data LBound = StrictLB    Version -- ^ greater than (>)
            | NonstrictLB Version -- ^ greater than or equal to (>=)
            | ZeroB               -- ^ no lower bound
    deriving (Eq, Show)

instance NFData LBound where
  rnf (StrictLB v) = rnf v
  rnf (NonstrictLB v) = rnf v
  rnf ZeroB = ()

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
-- | Type of upper bound of a dependency.
data UBound = StrictUB Version    -- ^ less than (<)
            | NonstrictUB Version -- ^ less than or equal to (<=)
            | InfinityB           -- ^ no upper bound
    deriving (Eq, Show)

instance NFData UBound where
  rnf (StrictUB v) = rnf v
  rnf (NonstrictUB v) = rnf v
  rnf InfinityB = ()

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

-- | Type of dependency version.
--
-- A dependency version may either be an exact 'Version' or a
-- version range between a given 'LBound' and 'UBound'.
data DRange = DRange LBound UBound
            | DExact Version
    deriving (Eq, Show, Ord)

instance NFData DRange where
  rnf (DRange l u) = rnf l `seq` rnf u
  rnf (DExact v) = rnf v

range_is_case_of :: DRange -> DRange -> Bool
range_is_case_of (DRange llow lup) (DRange rlow rup)
   | llow >= rlow && lup <= rup = True
range_is_case_of _ _ = False

-- | True if left 'DRange' covers at least as much as the right 'DRange'.
range_as_broad_as :: DRange -> DRange -> Bool
range_as_broad_as (DRange llow lup) (DRange rlow rup)
    | llow <= rlow && lup >= rup = True
range_as_broad_as _ _ = False

data DAttr = DAttr SlotDepend [UseFlag]
    deriving (Eq, Show, Ord)

instance NFData DAttr where
  rnf (DAttr sd uf) = rnf sd `seq` rnf uf

data Dependency = DependAtom Atom
                | DependAnyOf         [Dependency]
                | DependAllOf         [Dependency]
                | DependIfUse Use      Dependency Dependency -- u? ( td ) !u? ( fd )
    deriving (Eq, Show, Ord)

instance NFData Dependency where
  rnf (DependAtom a) = rnf a
  rnf (DependAnyOf ds) = rnf ds
  rnf (DependAllOf ds) = rnf ds
  rnf (DependIfUse u d d') = rnf u `seq` rnf d `seq` rnf d'

data Atom = Atom PackageName DRange DAttr deriving (Eq, Show, Ord)

instance NFData Atom where
  rnf (Atom pn dr da) = rnf pn `seq` rnf dr `seq` rnf da

-- | True if left 'Dependency' constraint is the same as (or looser than) right
-- 'Dependency' constraint.
dep_as_broad_as :: Dependency -> Dependency -> Bool
dep_as_broad_as l r
    -- very broad (not only on atoms) special case
    | l == r = True
-- atoms
dep_as_broad_as (DependAtom (Atom lpn lr lda)) (DependAtom (Atom rpn rr rda))
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
        DependAtom _
            -> False

dep_is_case_of :: Dependency -> Dependency -> Bool
dep_is_case_of l r
    -- very broad (not only on atoms) special case
    | l == r = True
-- only on atoms
dep_is_case_of (DependAtom (Atom lpn lr lda)) (DependAtom (Atom rpn rr rda))
    | lpn == rpn && lda == rda = lr `range_is_case_of` rr
dep_is_case_of _ _ = False
