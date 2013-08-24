module Portage.Dependency
  ( DRange(..)
  , DAttr(..)
  , LBound(..)
  , UBound(..)
  , Dependency(..)
  , SlotDepend(..)
  , simplify_deps
  , simplifyUseDeps
  , addDepUseFlag
  , setSlotDep
  , sortDeps
  , dep2str
  ) where

import Portage.Version
import Portage.Use

import Portage.PackageId

import Distribution.Text ( Text(..) )
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ( (<>), vcat, nest, render )

import Data.Function ( on )
import Data.Maybe ( fromJust, mapMaybe )
import Data.List ( groupBy, partition, sortBy )

data SlotDepend = AnySlot          -- nothing special
                | AnyBuildTimeSlot -- ':='
                | GivenSlot String -- ':slotno'
    deriving (Eq, Show)

dispSlot :: SlotDepend -> Disp.Doc
dispSlot AnySlot          = Disp.empty
dispSlot AnyBuildTimeSlot = Disp.text ":="
dispSlot (GivenSlot slot) = Disp.text (':' : slot)

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

dispLBound :: PackageName -> LBound -> Disp.Doc
dispLBound pn (StrictLB    v) = Disp.char '>' <> disp pn <-> disp v
dispLBound pn (NonstrictLB v) = Disp.text ">=" <> disp pn <-> disp v
dispLBound _pn ZeroB = error "unhandled 'dispLBound ZeroB'"

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

dispUBound :: PackageName -> UBound -> Disp.Doc
dispUBound pn (StrictUB    v) = Disp.char '<' <> disp pn <-> disp v
dispUBound pn (NonstrictUB v) = Disp.text "<=" <> disp pn <-> disp v
dispUBound _pn InfinityB = error "unhandled 'dispUBound Infinity'"

data DRange = DRange LBound UBound
            | DExact Version
    deriving (Eq, Show)

mergeDRanges :: DRange -> DRange -> DRange
mergeDRanges _ r@(DExact _) = r
mergeDRanges l@(DExact _) _ = l
mergeDRanges (DRange ll lu) (DRange rl ru) = DRange (max ll rl) (min lu ru)

data DAttr = DAttr SlotDepend [UseFlag]
    deriving (Eq, Show)

dispDAttr :: DAttr -> Disp.Doc
dispDAttr (DAttr s u) = dispSlot s <> dispUses u

data Dependency = Atom PackageName DRange DAttr
                | DependIfUse UseFlag Dependency
                | DependAnyOf         [Dependency]
                | DependAllOf         [Dependency]
    deriving (Eq, Show)

empty_dependency :: Dependency
empty_dependency = DependAllOf []

is_empty_dependency :: Dependency -> Bool
is_empty_dependency (DependIfUse _use dep)  =     is_empty_dependency dep
is_empty_dependency (DependAnyOf deps)      = all is_empty_dependency deps
is_empty_dependency (DependAllOf deps)      = all is_empty_dependency deps
is_empty_dependency (Atom _pn _dr _dattr)   = False

-- remove one layer of redundancy
normalization_step :: Dependency -> Dependency
normalization_step = combine_atoms . flatten . remove_empty
remove_empty :: Dependency -> Dependency
remove_empty d =
    case d of
        -- drop full empty nodes
        _ | is_empty_dependency d -> empty_dependency
        -- drop partial empty nodes
        (DependAnyOf deps)        -> DependAnyOf $ filter (not . is_empty_dependency) deps
        (DependAllOf deps)        -> DependAllOf $ filter (not . is_empty_dependency) deps
        -- no change
        _                         -> d

-- TODO: implement flatting (if not done yet in other phases)
--   DependAnyOf [DependAnyOf [something], rest] -> DependAnyOf $ something ++ rest
--   DependAllOf [DependAllOf [something], rest] -> DependAllOf $ something ++ rest
flatten :: Dependency -> Dependency
flatten = id

-- TODO: join atoms with different version constraints
-- DependAllOf [ DRange ">=foo-1" Inf, Drange Zero "<foo-2" ] -> DRange ">=foo-1" "<foo-2"
combine_atoms :: Dependency -> Dependency
combine_atoms d =
    case d of
        (DependIfUse use dep) -> DependIfUse use (combine_atoms dep)
        (DependAllOf deps)    -> DependAllOf $ map combine_atoms $ find_intersections  deps
        (DependAnyOf deps)    -> DependAnyOf $ map combine_atoms $ find_concatenations deps
        (Atom _pn _dr _dattr) -> d

find_intersections :: [Dependency] -> [Dependency]
find_intersections = map merge_depends . groupBy is_mergeable

-- TODO
find_concatenations :: [Dependency] -> [Dependency]
find_concatenations = id

-- remove various types of redundancy
normalize_depend :: Dependency -> Dependency
normalize_depend d = d''
    where d'  = normalization_step d
          d'' | d == d'   =                    d
              | otherwise = normalization_step d

-- TODO: be able to merge
--     [use? ( a ), use? ( b ) ] -> use? ( a b )
is_mergeable :: Dependency -> Dependency -> Bool
is_mergeable (Atom lpn _ldrange lattr) (Atom rpn _rdrange rattr) = (lpn, lattr) == (rpn, rattr)
is_mergeable _                         _                         = False

merge_depends :: [Dependency] -> Dependency
merge_depends [x] = x
merge_depends xs = foldl1 merge_pair xs

merge_pair :: Dependency -> Dependency -> Dependency
merge_pair (Atom lp ld la) (Atom rp rd ra)
    | lp /= rp = error "merge_pair got different 'PackageName's"
    | la /= ra = error "merge_pair got different 'DAttr's"
    | otherwise = Atom lp (mergeDRanges ld rd) la
merge_pair l r = error $ unwords ["merge_pair can't merge non-atoms:", show l, show r]

dep2str :: Int -> Dependency -> String
dep2str start_indent = render . nest start_indent . showDepend . normalize_depend

(<->) :: Disp.Doc -> Disp.Doc -> Disp.Doc
a <-> b = a <> Disp.char '-' <> b

sp :: Disp.Doc
sp = Disp.char ' '

sparens :: Disp.Doc -> Disp.Doc
sparens doc = Disp.parens (sp <> valign doc <> sp)

valign :: Disp.Doc -> Disp.Doc
valign d = nest 0 d

showDepend :: Dependency -> Disp.Doc
showDepend (Atom pn range dattr)
    = case range of
        -- any version
        DRange ZeroB InfinityB -> disp pn          <> dispDAttr dattr
        DRange ZeroB ub        -> dispUBound pn ub <> dispDAttr dattr
        DRange lb InfinityB    -> dispLBound pn lb <> dispDAttr dattr
        -- TODO: handle >=foo-0    special case
        -- TODO: handle =foo-x.y.* special case
        DRange lb ub          ->    showDepend (Atom pn (DRange lb InfinityB) dattr)
                                 <> Disp.char ' '
                                 <> showDepend (Atom pn (DRange ZeroB ub)    dattr)
        DExact v              -> Disp.char '~' <> disp pn <-> disp v { versionRevision = 0 } <> dispDAttr dattr

showDepend (DependIfUse u dep)  = disp u         <> sp <> sparens (showDepend dep)
showDepend (DependAnyOf deps)   = Disp.text "||" <> sp <> sparens (vcat $ map showDependInAnyOf deps)
showDepend (DependAllOf deps)   = valign $ vcat $ map showDepend deps

-- needs special grouping
showDependInAnyOf :: Dependency -> Disp.Doc
showDependInAnyOf d@(DependAllOf _deps) = sparens (showDepend d)
-- both lower and upper bounds are present thus needs 2 atoms
-- TODO: '=foo-x.y.*' will take only one atom, not two
showDependInAnyOf d@(Atom _pn (DRange lb ub) _dattr)
    | lb /= ZeroB && ub /= InfinityB
                                       = sparens (showDepend d)
-- rest are fine
showDependInAnyOf d                    =          showDepend d

-- TODO: remove it in favour of more robust 'normalize_depend'
simplify_deps :: [Dependency] -> [Dependency]
simplify_deps = id

getPackage :: Dependency -> Maybe PackageName
getPackage (DependAllOf _dependency) = Nothing
getPackage (Atom pn _dr _attrs) = Just pn
getPackage (DependAnyOf _dependency           ) = Nothing
getPackage (DependIfUse  _useFlag    _Dependency) = Nothing

setSlotDep :: SlotDepend -> Dependency -> Dependency
setSlotDep n (DependAllOf d) = DependAllOf $ map (setSlotDep n) d
setSlotDep n (Atom pn dr (DAttr _s u)) = Atom pn dr (DAttr n u)
setSlotDep n (DependAnyOf d) = DependAnyOf $ map (setSlotDep n) d
setSlotDep n (DependIfUse u d) = DependIfUse u (setSlotDep n d)

addDepUseFlag :: UseFlag -> Dependency -> Dependency
addDepUseFlag n (DependAllOf d) = DependAllOf $ map (addDepUseFlag n) d
addDepUseFlag n (Atom pn dr (DAttr s u)) = Atom pn dr (DAttr s (n:u))
addDepUseFlag n (DependAnyOf d) = DependAnyOf $ map (addDepUseFlag n) d
addDepUseFlag n (DependIfUse u d) = DependIfUse u (addDepUseFlag n d)

--
-- | remove all Use dependencies that overlap with normal dependencies
simplifyUseDeps :: [Dependency]         -- list where use deps is taken
                    -> [Dependency]     -- list where common deps is taken
                    -> [Dependency]     -- result deps
simplifyUseDeps ds cs =
    let (u,o) = partition isUseDep ds
        c = mapMaybe getPackage cs
    in (mapMaybe (intersectD c) u)++o

intersectD :: [PackageName] -> Dependency -> Maybe Dependency
intersectD fs (DependIfUse u d) = intersectD fs d >>= Just . DependIfUse u
intersectD fs (DependAnyOf ds) =
    let ds' = mapMaybe (intersectD fs) ds
    in if null ds' then Nothing else Just (DependAnyOf ds')
intersectD fs (DependAllOf ds) =
    let ds' = mapMaybe (intersectD fs) ds
    in if null ds' then Nothing else Just (DependAllOf ds')
intersectD fs x =
    let pkg = fromJust $ getPackage x -- this is unsafe but will save from error later
    in if any (==pkg) fs then Nothing else Just x

isUseDep :: Dependency -> Bool
isUseDep (DependIfUse _ _) = True
isUseDep _ = False


sortDeps :: [Dependency] -> [Dependency]
sortDeps = sortBy dsort . map deeper
  where
    deeper :: Dependency -> Dependency
    deeper (DependIfUse u1 d) = DependIfUse u1 $ deeper d
    deeper (DependAllOf ds)   = DependAllOf $ sortDeps ds
    deeper (DependAnyOf ds)  = DependAnyOf $ sortDeps ds
    deeper x = x
    dsort :: Dependency -> Dependency -> Ordering
    dsort (DependIfUse u1 _) (DependIfUse u2 _) = u1 `compare` u2
    dsort (DependIfUse _ _)  (DependAnyOf _)   = LT
    dsort (DependIfUse _ _)  (DependAllOf  _)   = LT
    dsort (DependIfUse _ _)  _                  = GT
    dsort (DependAnyOf _)   (DependAnyOf _)   = EQ
    dsort (DependAnyOf _)  (DependIfUse _ _)   = GT
    dsort (DependAnyOf _)   (DependAllOf _)    = LT
    dsort (DependAnyOf _)   _                  = GT
    dsort (DependAllOf _)    (DependAllOf _)    = EQ
    dsort (DependAllOf _)    (DependIfUse  _ _) = LT
    dsort (DependAllOf _)    (DependAnyOf _)   = GT
    dsort _ (DependIfUse _ _)                   = LT
    dsort _ (DependAllOf _)                     = LT
    dsort _ (DependAnyOf _)                    = LT
    dsort a b = (compare `on` getPackage) a b
