module Portage.Dependency.Normalize
  (
    normalize_depend
  ) where

import           Control.Monad
import qualified Data.List as L
import           Data.Maybe

import Portage.Dependency.Types
import Portage.Dependency.Builder
import Portage.Use

import Debug.Trace

mergeDRanges :: DRange -> DRange -> DRange
mergeDRanges _ r@(DExact _) = r
mergeDRanges l@(DExact _) _ = l
mergeDRanges (DRange ll lu) (DRange rl ru) = DRange (max ll rl) (min lu ru)

-- remove one layer of redundancy
normalization_step :: Dependency -> Dependency
normalization_step = combine_atoms
                   . propagate_context
                   . flatten
                   . lift_context
                   . remove_duplicates
                   . remove_empty
                   . sort_deps
                   . combine_use_guards
                   . combine_use_counterguards

remove_empty :: Dependency -> Dependency
remove_empty d =
    case d of
        -- drop full empty nodes
        _ | is_empty_dependency d -> empty_dependency
        -- drop partial empty nodes
        DependIfUse use td fd   -> DependIfUse use (remove_empty td) (remove_empty fd)
        DependAllOf deps        -> DependAllOf $ filter (not . is_empty_dependency) $ map remove_empty deps
        DependAnyOf deps        -> DependAnyOf $                                      map remove_empty deps
        -- no change
        Atom _pn _dr _dattr     -> d

-- Ideally 'combine_atoms' should handle those as well
remove_duplicates :: Dependency -> Dependency
remove_duplicates d =
    case d of
        DependIfUse use td fd   -> DependIfUse use (remove_duplicates td) (remove_duplicates fd)
        DependAnyOf deps        -> DependAnyOf $ L.nub $ map remove_duplicates deps
        DependAllOf deps        -> DependAllOf $ L.nub $ map remove_duplicates deps
        Atom _pn _dr _dattr     -> d

-- TODO: implement flattening AnyOf the same way it's done for AllOf
--   DependAnyOf [DependAnyOf [something], rest] -> DependAnyOf $ something ++ rest
flatten :: Dependency -> Dependency
flatten d =
    case d of
        DependIfUse use td fd   -> DependIfUse use (go td) (go fd)
        DependAnyOf [dep]       -> go dep
        DependAnyOf deps        -> DependAnyOf $ map go deps

        DependAllOf deps        -> case L.partition is_dall_of (map go deps) of
                                       ([], [])      -> empty_dependency
                                       ([], [dep])   -> dep
                                       ([], ndall)   -> DependAllOf ndall
                                       (dall, ndall) -> go $ DependAllOf $ (concatMap undall dall) ++ ndall
        Atom _pn _dr _dattr     -> d
  where go :: Dependency -> Dependency
        go = flatten

        is_dall_of :: Dependency -> Bool
        is_dall_of d' =
            case d' of
                DependAllOf _deps -> True
                _                 -> False
        undall :: Dependency -> [Dependency]
        undall ~(DependAllOf ds) = ds

-- joins atoms with different version boundaries
-- DependAllOf [ DRange ">=foo-1" Inf, Drange Zero "<foo-2" ] -> DRange ">=foo-1" "<foo-2"
combine_atoms :: Dependency -> Dependency
combine_atoms d =
    case d of
        DependIfUse use td fd -> DependIfUse use (combine_atoms td) (combine_atoms fd)
        DependAllOf deps      -> DependAllOf $ map combine_atoms $ find_atom_intersections  deps
        DependAnyOf deps      -> DependAnyOf $ map combine_atoms $ find_atom_concatenations deps
        Atom _pn _dr _dattr   -> d

find_atom_intersections :: [Dependency] -> [Dependency]
find_atom_intersections = map merge_depends . L.groupBy is_mergeable
    where is_mergeable :: Dependency -> Dependency -> Bool
          is_mergeable (Atom lpn _ldrange lattr) (Atom rpn _rdrange rattr) = (lpn, lattr) == (rpn, rattr)
          is_mergeable _                         _                         = False

          merge_depends :: [Dependency] -> Dependency
          merge_depends [x] = x
          merge_depends xs = L.foldl1' merge_pair xs

          merge_pair :: Dependency -> Dependency -> Dependency
          merge_pair (Atom lp ld la) (Atom rp rd ra)
              | lp /= rp = error "merge_pair got different 'PackageName's"
              | la /= ra = error "merge_pair got different 'DAttr's"
              | otherwise = Atom lp (mergeDRanges ld rd) la
          merge_pair l r = error $ unwords ["merge_pair can't merge non-atoms:", show l, show r]

-- TODO
find_atom_concatenations :: [Dependency] -> [Dependency]
find_atom_concatenations = id

-- Eliminate use guarded redundancy:
--   a? ( foo )
--   a? ( bar )
-- gets translated to
--   a? ( foo bar )
combine_use_guards :: Dependency -> Dependency
combine_use_guards d =
    case d of
        DependIfUse use td fd -> DependIfUse use (combine_use_guards td) (combine_use_guards fd)
        DependAllOf deps      -> DependAllOf $ map combine_use_guards $ find_use_intersections  deps
        DependAnyOf deps      -> DependAnyOf $ map combine_use_guards $ find_use_concatenations deps
        Atom _pn _dr _dattr   -> d

find_use_intersections :: [Dependency] -> [Dependency]
find_use_intersections = map merge_use_intersections . L.groupBy is_use_mergeable
    where
        is_use_mergeable :: Dependency -> Dependency -> Bool
        is_use_mergeable (DependIfUse lu _ltd _lfd) (DependIfUse ru _rtd _rfd)
            | lu == ru       = True
        is_use_mergeable _ _ = False

        merge_use_intersections :: [Dependency] -> Dependency
        merge_use_intersections [x] = x
        merge_use_intersections ds = DependIfUse u (DependAllOf tds) (DependAllOf fds)
            where DependIfUse u _tf _fd = head ds
                  tfdeps ~(DependIfUse _u td fd) = (td, fd)
                  (tds, fds) = unzip $ map tfdeps ds

-- TODO
find_use_concatenations :: [Dependency] -> [Dependency]
find_use_concatenations = id

-- Eliminate use guarded redundancy:
--   a? ( foo bar )
--   !a? ( foo baz )
-- gets translated to
--   foo
--   a? ( bar )
--   !a? ( baz )
combine_use_counterguards :: Dependency -> Dependency
combine_use_counterguards d =
    case d of
        DependIfUse use td fd -> pop_common $ DependIfUse use (combine_use_counterguards td) (combine_use_counterguards fd)
        DependAllOf deps      -> DependAllOf $ map combine_use_counterguards deps
        DependAnyOf deps      -> DependAnyOf $ map combine_use_counterguards deps
        Atom _pn _dr _dattr   -> d
    where pop_common :: Dependency -> Dependency
          -- depend
          --   a? ( x ) !a? ( x )
          -- gets translated to
          --   x
          pop_common (DependIfUse _u td fd)
              | td == fd = fd
          pop_common d'@(DependIfUse _u td fd) =
              case td_ctx `L.intersect` fd_ctx of
                  [] -> d'
                  -- TODO: force simplification right there
                  common_ctx -> DependAllOf $ propagate_context' common_ctx d' : common_ctx
              where td_ctx = lift_context' td
                    fd_ctx = lift_context' fd
          pop_common x = x

-- Eliminate top-down redundancy:
--   foo/bar
--   u? ( foo/bar
--        bar/baz )
-- gets translated to
--   foo/bar
--   u? ( bar/baz )
propagate_context :: Dependency -> Dependency
propagate_context = propagate_context' []

-- very simple model: pick all sibling-atom deps and add them to context
--                    for downward proparation and remove from 'all_of' part
-- TODO: any-of part can benefit from it by removing unsatisfiable or satisfied alternative
propagate_context' :: [Dependency] -> Dependency -> Dependency
propagate_context' ctx d =
    case d of
        DependIfUse use td fd -> DependIfUse use (go (refine_context (True,  use) ctx) td)
                                                 (go (refine_context (False, use) ctx) fd)
        DependAllOf deps      -> DependAllOf $ fromJust $ msum $
                                                   [ v
                                                   | (optimized_d, other_deps) <- slice_list deps
                                                   , let ctx' = ctx ++ other_deps
                                                         d'   = propagate_context' ctx' optimized_d
                                                         v    = case d' /= optimized_d of
                                                                    True  -> Just (d':other_deps)
                                                                    False -> Nothing -- haven't managed to optimize anything
                                                   ] ++ [Just deps] -- unmodified
        DependAnyOf deps      -> DependAnyOf $ map (go ctx) deps
        Atom _pn _dr _dattr   -> case any (dep_as_broad_as d) ctx of
                                     True  -> empty_dependency
                                     False -> d
  where go c = propagate_context' c

refine_context :: (Bool, Use) -> [Dependency] -> [Dependency]
refine_context use_cond = map (flatten . refine_ctx_unit use_cond)
    where refine_ctx_unit :: (Bool, Use) -> Dependency -> Dependency
          refine_ctx_unit uc@(bu, u) d =
              case d of
                DependIfUse u' td fd
                  -> case u == u' of
                         False -> DependIfUse u' (refine_ctx_unit uc td)
                                                 (refine_ctx_unit uc fd)
                         True  -> refine_ctx_unit uc $ if bu
                                                           then td
                                                           else fd
                _ -> d

-- generates all pairs of:
-- (list_element, list_without_element)
-- example:
--   [1,2,3]
-- yields
--   [(1, [2,3]), (2,[1,3]), (3,[1,2])]
slice_list :: [e] -> [(e, [e])]
slice_list [] = []
slice_list (e:es) = (e, es) : map (\(v, vs) -> (v, e : vs)) (slice_list es)

-- Eliminate bottom-up redundancy:
--   || ( ( foo/bar bar/baz )
--        ( foo/bar bar/quux ) )
-- gets translated to
--   foo/bar
--   || ( ( foo/bar bar/baz )
--        ( foo/bar bar/quux ) )
-- It looks like became more gross,
-- but 'propagate_context' phase
-- cleanups it to the following state:
--   foo/bar
--   || ( bar/baz
--        bar/quux )
-- TODO: better add propagation in this exact plase to keep tree shrinking only
lift_context :: Dependency -> Dependency
lift_context d =
    case d of
        DependIfUse _use _td _fd -> case L.delete d new_ctx of
                                        []       -> d
                                        new_ctx' -> propagate_context $ DependAllOf $ d : new_ctx'
        DependAllOf deps         -> case new_ctx L.\\ deps of
                                        []       -> d
                                        new_ctx' -> DependAllOf $ deps ++ new_ctx'
        -- the lift itself
        DependAnyOf _deps        -> case L.delete d new_ctx of
                                         []       -> d
                                         new_ctx' -> propagate_context $ DependAllOf $ d : new_ctx'
        Atom _pn _dr _dattr      -> d
  where new_ctx = lift_context' d

-- lift everything that can be shared somewhere else
-- propagate_context will then pick some bits from here
-- and remove them deep inside.
-- It's the most fragile and powerfull pass
lift_context' :: Dependency -> [Dependency]
lift_context' d =
    case d of
        DependIfUse _use td fd   -> d : extract_common_constraints (map lift_context' [td, fd])
        DependAllOf deps         -> L.nub $ concatMap lift_context' deps
        DependAnyOf deps         -> extract_common_constraints $ map lift_context' deps
        Atom _pn _dr _dattr      -> [d]

-- it extracts common part of dependency comstraints.
-- Some examples:
--  'a b c' and 'b c d' have common 'b c'
--  'u? ( a  b )' and 'u? ( b c )' have common 'u? ( b )' part
--  'a? ( b? ( x y ) )' and !a? ( b? ( y z ) )' have common 'b? ( y )'
extract_common_constraints :: [[Dependency]] -> [Dependency]
extract_common_constraints [] = []
extract_common_constraints dss@(ds:dst) = common_atoms ++ common_use_guards
    where common_atoms :: [Dependency]
          common_atoms = L.foldl1' L.intersect dss
          common_use_guards :: [Dependency]
          common_use_guards = [ DependIfUse u (DependAllOf tdi) (DependAllOf fdi)
                              | DependIfUse u td fd <- ds
                              , Just (tds, fds) <- [find_matching_use_deps dst u ([lift_context' td], [lift_context' fd])]
                              , let tdi = extract_common_constraints tds
                                    fdi = extract_common_constraints fds
                              , not (null tdi && null fdi)
                              ]

find_matching_use_deps :: [[Dependency]] -> Use -> ([[Dependency]], [[Dependency]]) -> Maybe ([[Dependency]], [[Dependency]])
find_matching_use_deps dss u (tds, fds) =
    case dss of
        []       -> Just (tds, fds)
        (ds:dst) -> case [ (tc, fc)
                         | DependIfUse u' td fd <- ds
                         , u' == u
                         , let tc = lift_context' td
                               fc = lift_context' fd
                         , not (null tc && null fc)
                         ] of
                        []    -> Nothing
                        pairs -> find_matching_use_deps dst u (map fst pairs ++ tds, map snd pairs ++ fds)

-- reorders depends to make them more attractive
-- for other normalization algorithms
-- and for final pretty-printer
sort_deps :: Dependency -> Dependency
sort_deps d =
    case d of
        DependIfUse lu lt lf
            | is_empty_dependency lf ->
                case lt of
                    DependIfUse ru rt rf
                        -- b? ( a? ( d ) )
                        | ru < lu && is_empty_dependency rf -> mkUseDependency (True,  ru) $ mkUseDependency (True, lu) (sort_deps rt)
                        -- b? ( !a? ( d ) )
                        | ru < lu && is_empty_dependency rt -> mkUseDependency (False, ru) $ mkUseDependency (True, lu) (sort_deps rf)
                    _ -> DependIfUse lu (sort_deps lt) (sort_deps lf)
            | is_empty_dependency lt ->
                case lf of
                    DependIfUse ru rt rf
                        -- !b? ( a? ( d ) )
                        | ru < lu && is_empty_dependency rf -> mkUseDependency (True,  ru) $ mkUseDependency (False, lu) (sort_deps rt)
                        -- !b? ( !a? ( d ) )
                        | ru < lu && is_empty_dependency rt -> mkUseDependency (False, ru) $ mkUseDependency (False, lu) (sort_deps rf)
                    _ -> DependIfUse lu (sort_deps lt) (sort_deps lf)
        DependIfUse use td fd   -> DependIfUse use (sort_deps td) (sort_deps fd)
        DependAnyOf deps        -> DependAnyOf $ L.sort $ map sort_deps deps
        DependAllOf deps        -> DependAllOf $ L.sort $ map sort_deps deps
        Atom _pn _dr _dattr     -> d

-- remove various types of redundancy
normalize_depend :: Dependency -> Dependency
normalize_depend = normalize_depend' 50 -- arbitrary limit

normalize_depend' :: Int -> Dependency -> Dependency
normalize_depend' 0     d = trace "WARNING: Normalize_depend hung up. Optimization is incomplete." d
normalize_depend' level d = next_step next_d
    where next_d = normalization_step d
          next_step | d == next_d = id
                    | otherwise   = normalize_depend' (level - 1)
