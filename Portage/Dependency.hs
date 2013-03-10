module Portage.Dependency
  ( Dependency(..)
  , SlotDepend(..)
  , simplify_deps
  , simplifyUseDeps
  , addDepUseFlag
  , setSlotDep
  , sortDeps
  ) where

import Portage.Version
import Portage.Use
import Distribution.Text ( display, Text(..) )

import Portage.PackageId

import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ( (<>), hsep )

import Data.Function ( on )
import Data.Maybe ( fromJust, catMaybes, mapMaybe )
import Data.List ( nub, groupBy, partition, sortBy, sort )
import Data.Ord           (comparing, Ordering(..))

data SlotDepend = AnySlot          -- nothing special
                | AnyBuildTimeSlot -- ':='
                | GivenSlot String -- ':slotno'
    deriving (Eq, Show)

dispSlot :: SlotDepend -> Disp.Doc
dispSlot AnySlot          = Disp.empty
dispSlot AnyBuildTimeSlot = Disp.text ":="
dispSlot (GivenSlot slot) = Disp.text (':' : slot)

data Dependency = AnyVersionOf               PackageName SlotDepend [UseFlag]
                | ThisVersionOf      Version PackageName SlotDepend [UseFlag]  -- ~package-version
                | LaterVersionOf     Version PackageName SlotDepend [UseFlag]  -- >package-version
                | EarlierVersionOf   Version PackageName SlotDepend [UseFlag]  -- <package-version
                | OrLaterVersionOf   Version PackageName SlotDepend [UseFlag]  -- >=package-version
                | OrEarlierVersionOf Version PackageName SlotDepend [UseFlag]  -- <=package-version
                | ThisMajorOf        Version PackageName SlotDepend [UseFlag]  -- =package-version*
                | DependIfUse  UseFlag    Dependency                -- use? ( depend )
                | DependEither [Dependency]                         -- || ( depend1 depend2 ... )
                | AllOf        [Dependency]                         -- ( depend1 depend2 ... )
    deriving (Eq,Show)

instance Text Dependency where
  disp = showDepend

(<->) :: Disp.Doc -> Disp.Doc -> Disp.Doc
a <-> b = a <> Disp.char '-' <> b

showDepend :: Dependency -> Disp.Doc
showDepend (AnyVersionOf         p s u) = disp p <> dispSlot s <> dispUses u
showDepend (ThisVersionOf      v p s u) = Disp.char '~' <> disp p <-> disp v { versionRevision = 0 } <> dispSlot s <> dispUses u
showDepend (LaterVersionOf     v p s u) = Disp.char '>' <> disp p <-> disp v <> dispSlot s <> dispUses u
showDepend (EarlierVersionOf   v p s u) = Disp.char '<' <> disp p <-> disp v <> dispSlot s <> dispUses u
showDepend (OrLaterVersionOf   v p s u) = Disp.text ">=" <> disp p <-> disp v <> dispSlot s <> dispUses u
showDepend (OrEarlierVersionOf v p s u) = Disp.text "<=" <> disp p <-> disp v <> dispSlot s <> dispUses u
showDepend (ThisMajorOf        v p s u) = Disp.char '='  <> disp p <-> disp v <> Disp.char '*' <> dispSlot s <> dispUses u
showDepend (DependEither       dp ) = Disp.text "|| ( " <> hsep (map showDepend dp) <> Disp.text " )"
showDepend (DependIfUse        useflag dep) = disp useflag <> Disp.text " " <> pp_deps dep
    where -- special case to avoid double braces: test? ( ( ) )
          pp_deps (AllOf _) =                               disp dep
          pp_deps         _ = Disp.parens (Disp.text " " <> disp dep <> Disp.text " ")
showDepend (AllOf []) = Disp.empty
showDepend (AllOf              (d:dp) ) =
    Disp.text "( " <> showDepend d <> line
    <> Disp.hcat (map (\x -> Disp.text "\t\t\t" <> (showDepend x) <> line) dp)
    <> Disp.text "\t\t)"
    where line = Disp.char '\n'

{- Here goes code for dependencies simplification -}

simplify_group_table :: PackageName ->
                        SlotDepend  ->
                        [UseFlag]   ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version -> [Dependency]

-- simplify_group_table p ol       l        e        oe       exact
-- 1) trivial cases:
simplify_group_table    p _s _u Nothing  Nothing  Nothing  Nothing  Nothing  = error $ "Portage/Dependency.hs: " ++ display p ++ ": unsolvable constraints"
simplify_group_table    p s u (Just v) Nothing  Nothing  Nothing  Nothing  = [OrLaterVersionOf v p s u]
simplify_group_table    p s u Nothing  (Just v) Nothing  Nothing  Nothing  = [LaterVersionOf v p s u]
simplify_group_table    p s u Nothing  Nothing  (Just v) Nothing  Nothing  = [EarlierVersionOf v p s u]
simplify_group_table    p s u Nothing  Nothing  Nothing  (Just v) Nothing  = [OrEarlierVersionOf v p s u]
simplify_group_table    p s u Nothing  Nothing  Nothing  Nothing  (Just v) = [ThisVersionOf v p s u]

-- 2) simplification passes
simplify_group_table    p s u (Just (Version v1 _ _ _)) Nothing (Just (Version v2 _ _ _)) Nothing Nothing
    -- special case: >=a-v.N a<v.(N+1)   => =a-v.N*
    | (init v1 == init v2) && (last v2 == last v1 + 1) = [ThisMajorOf (Version v1 Nothing [] 0) p s u]
    | otherwise                                        = [OrLaterVersionOf (Version v1 Nothing [] 0) p s u, EarlierVersionOf (Version v2 Nothing [] 0) p s u]

-- TODO: simplify constraints of type: >=a-v1; > a-v2 and such

-- 3) otherwise sink:
simplify_group_table    p s u (Just v)     l@(_)       e@(_)        oe@(_)       exact@(_) =   OrLaterVersionOf v p s u: simplify_group_table p s u Nothing  l e oe exact
simplify_group_table    p s u ol@(Nothing) (Just v)    e@(_)        oe@(_)       exact@(_) =     LaterVersionOf v p s u: simplify_group_table p s u ol Nothing e oe exact
simplify_group_table    p s u ol@(Nothing) l@(Nothing) (Just v)     oe@(_)       exact@(_) =   EarlierVersionOf v p s u: simplify_group_table p s u ol l Nothing oe exact
simplify_group_table    p s u ol@(Nothing) l@(Nothing) e@(Nothing)  (Just v)     exact@(_) = OrEarlierVersionOf v p s u: simplify_group_table p s u ol l e Nothing  exact
-- already defined earlier
-- simplify_group_table    p s u ol@(Nothing) l@(Nothing) e@(Nothing)  oe@(Nothing) (Just v)  = OrEarlierVersionOf v p : simplify_group_table p ol l e oe Nothing

--  >a-v1 >a-v2         => >a-(max v1 v2)
-- key idea: all constraints are enforcing constraints, so we can't get
-- more, than one interval.
simplify_group :: [Dependency] -> [Dependency]
simplify_group [dep@(AnyVersionOf _package _s _u)] = [dep]
simplify_group [dep@(ThisMajorOf _v    _p _s _u)]  = [dep]
simplify_group deps = simplify_group_table package
                                           slot
                                           uses
                                           min_or_later_v   -- >=
                                           min_later_v      -- >
                                           max_earlier_v    -- <
                                           max_or_earlier_v -- <=
                                           exact_this_v     -- ==
    where
          package = fromJust.getPackage $ head deps
          slot    = fromJust.getSlot    $ head deps
          uses    = fromJust.getUses    $ head deps
          max_earlier_v    = safe_minimum $ map earlier_v deps
          max_or_earlier_v = safe_minimum $ map or_earlier_v deps
          min_later_v      = safe_maximum $ map later_v deps
          min_or_later_v   = safe_maximum $ map or_later_v deps
          exact_this_v     = case catMaybes (map this_v deps) of
                                  []  -> Nothing
                                  [v] -> Just v
                                  xs  -> error $ "too many exact versions:" ++ show xs
          --
          earlier_v (EarlierVersionOf v _p _s _u) = Just v
          earlier_v _                       = Nothing

          or_earlier_v (OrEarlierVersionOf v _p _s _u) = Just v
          or_earlier_v _                         = Nothing

          later_v (LaterVersionOf v _p _s _u) = Just v
          later_v _                     = Nothing

          or_later_v (OrLaterVersionOf v _p _s _u) = Just v
          or_later_v _                     = Nothing

          this_v (ThisVersionOf v  _p _s _u) = Just v
          this_v _                     = Nothing
          --
          safe_minimum xs = case catMaybes xs of
                                 [] -> Nothing
                                 xs' -> Just $ minimum xs'
          safe_maximum xs = case catMaybes xs of
                                 [] -> Nothing
                                 xs' -> Just $ maximum xs'

-- divide packages to groups (by package name), simplify groups, merge again
simplify_deps :: [Dependency] -> [Dependency]
simplify_deps deps = flattenDep $ 
                        (concatMap (simplify_group.nub) $
                            groupBy cmpPkgName $
                                sortBy (comparing getPackagePart) groupable)
                        ++ ungroupable
    where (ungroupable, groupable) = partition ((==Nothing).getPackage) deps
          --
          cmpPkgName p1 p2 = cmpMaybe (getPackage p1) (getPackage p2)
          cmpMaybe (Just p1) (Just p2) = p1 == p2
          cmpMaybe _         _         = False
          --
          flattenDep :: [Dependency] -> [Dependency]
          flattenDep [] = []
          flattenDep (AllOf ds:xs) = (concatMap (\x -> flattenDep [x]) ds) ++ flattenDep xs
          flattenDep (x:xs) = x:flattenDep xs
          -- TODO concat 2 dep either in the same group

getPackage :: Dependency -> Maybe PackageName
getPackage (AllOf _dependency) = Nothing
getPackage (AnyVersionOf package _s _uses) = Just package
getPackage (ThisVersionOf      _version package _s _uses) = Just package
getPackage (LaterVersionOf     _version package _s _uses) = Just package
getPackage (EarlierVersionOf   _version package _s _uses) = Just package
getPackage (OrLaterVersionOf   _version package _s _uses) = Just package
getPackage (OrEarlierVersionOf _version package _s _uses) = Just package
getPackage (ThisMajorOf        _version package _s _uses) = Just package
getPackage (DependEither _dependency           ) = Nothing
getPackage (DependIfUse  _useFlag    _Dependency) = Nothing

getUses  :: Dependency -> Maybe [UseFlag]
getUses (AllOf _d) = Nothing
getUses (AnyVersionOf _p _s u) = Just u
getUses (ThisVersionOf _v _p _s u) = Just u
getUses (LaterVersionOf _v _p _s u) = Just u
getUses (EarlierVersionOf _v _p _s u) = Just u
getUses (OrLaterVersionOf _v _p _s u) = Just u
getUses (OrEarlierVersionOf _v _p _s u) = Just u
getUses (ThisMajorOf _v _p _s u) = Just u
getUses (DependEither _d) = Nothing
getUses (DependIfUse _u _d) = Nothing

getSlot :: Dependency -> Maybe SlotDepend
getSlot (AllOf _d) = Nothing
getSlot (AnyVersionOf _p s _u) = Just s
getSlot (ThisVersionOf _v _p s _u) = Just s
getSlot (LaterVersionOf _v _p s _u) = Just s
getSlot (EarlierVersionOf _v _p s _u) = Just s
getSlot (OrLaterVersionOf _v _p s _u) = Just s
getSlot (OrEarlierVersionOf _v _p s _u) = Just s
getSlot (ThisMajorOf _v _p s _u) = Just s
getSlot (DependEither _d) = Nothing
getSlot (DependIfUse _u _d) = Nothing

--
getPackagePart :: Dependency -> PackageName
getPackagePart dep = fromJust (getPackage dep)

--
setSlotDep :: SlotDepend -> Dependency -> Dependency
setSlotDep n (AllOf d) = AllOf $ map (setSlotDep n) d
setSlotDep n (AnyVersionOf p _s u) = AnyVersionOf p n u
setSlotDep n (ThisVersionOf v p _s u) = ThisVersionOf v p n u
setSlotDep n (LaterVersionOf v p _s u) = LaterVersionOf v p n u
setSlotDep n (EarlierVersionOf v p _s u) = EarlierVersionOf v p n u
setSlotDep n (OrLaterVersionOf v p _s u) = OrLaterVersionOf v p n u
setSlotDep n (OrEarlierVersionOf v p _s u) = OrEarlierVersionOf v p n u
setSlotDep n (ThisMajorOf v p _s u) = ThisMajorOf v p n u
setSlotDep n (DependEither d) = DependEither $ map (setSlotDep n) d
setSlotDep n (DependIfUse u d) = DependIfUse u (setSlotDep n d)

addDepUseFlag :: UseFlag -> Dependency -> Dependency
addDepUseFlag n (AllOf d) = AllOf $ map (addDepUseFlag n) d
addDepUseFlag n (AnyVersionOf p s u) = AnyVersionOf p s (n:u)
addDepUseFlag n (ThisVersionOf v p s u) = ThisVersionOf v p s (n:u)
addDepUseFlag n (LaterVersionOf v p s u) = LaterVersionOf v p s (n:u)
addDepUseFlag n (EarlierVersionOf v p s u) = EarlierVersionOf v p s (n:u)
addDepUseFlag n (OrLaterVersionOf v p s u) = OrLaterVersionOf v p s (n:u)
addDepUseFlag n (OrEarlierVersionOf v p s u) = OrEarlierVersionOf v p s (n:u)
addDepUseFlag n (ThisMajorOf v p s u) = ThisMajorOf v p s (n:u)
addDepUseFlag n (DependEither d) = DependEither $ map (addDepUseFlag n) d
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
intersectD fs (DependEither ds) =
    let ds' = mapMaybe (intersectD fs) ds
    in if null ds' then Nothing else Just (DependEither ds')
intersectD fs (AllOf ds) =
    let ds' = mapMaybe (intersectD fs) ds
    in if null ds' then Nothing else Just (AllOf ds')
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
    deeper (AllOf ds)         = AllOf  $ sortDeps ds
    deeper (DependEither ds)  = DependEither $ sortDeps ds
    deeper x = x
    dsort :: Dependency -> Dependency -> Ordering
    dsort (DependIfUse u1 _) (DependIfUse u2 _) = u1 `compare` u2
    dsort (DependIfUse _ _)  (DependEither _)   = LT
    dsort (DependIfUse _ _)  (AllOf  _)         = LT
    dsort (DependIfUse _ _)  _                  = GT
    dsort (DependEither _)   (DependEither _)   = EQ
    dsort (DependEither _)  (DependIfUse _ _)   = GT
    dsort (DependEither _)   (AllOf _)          = LT
    dsort (DependEither _)   _                  = GT
    dsort (AllOf _)    (AllOf _)                = EQ
    dsort (AllOf _)    (DependIfUse  _ _)       = LT
    dsort (AllOf _)    (DependEither _)         = GT
    dsort _ (DependIfUse _ _)                   = LT
    dsort _ (AllOf _)                           = LT
    dsort _ (DependEither _)                    = LT
    dsort a b = (compare `on` getPackage) a b
