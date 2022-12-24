
-- TODO: Rearrange things so we don't have to disable this warning
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Portage.Dependency.Normalize
  (
    normalize_depend
  ) where

import qualified Control.Arrow as A
import           Control.Monad
import qualified Data.List as L
import qualified Data.Set as S
import           Data.Maybe

import Portage.Dependency.Builder
import Portage.Dependency.Types
import Portage.Use

import Debug.Trace

mergeDRanges :: DRange -> DRange -> DRange
mergeDRanges _ r@(DExact _) = r
mergeDRanges l@(DExact _) _ = l
mergeDRanges (DRange ll lu) (DRange rl ru) = DRange (max ll rl) (min lu ru)

stabilize_pass :: (Dependency -> Dependency) -> Dependency -> Dependency
stabilize_pass pass d
    | d == d' = d'
    | otherwise = go d'
    where go = stabilize_pass pass
          d' = pass d

-- remove one layer of redundancy
normalization_step :: Int -> Dependency -> Dependency
normalization_step level =
      id
    . tp "PC2" (stabilize_pass (tp "PC2 step" propagate_context))
    . tp "F3" (stabilize_pass flatten)
    . tp "LC" lift_context
    . tp "PC1" (stabilize_pass (tp "PC1 step" propagate_context))
    . tp "F2" (stabilize_pass flatten)
    . tp "RD" (stabilize_pass remove_duplicates)
    . tp "RE" (stabilize_pass remove_empty)
    . tp "SD" sort_deps
    . tp "CUG" combine_use_guards
    . tp "F1" (stabilize_pass flatten)
    . tp "CAR" combine_atom_ranges
    where tp :: String -> (Dependency -> Dependency) -> Dependency -> Dependency
          tp pass_name pass d = t False d'
              where d' = pass d
                    t False = id
                    t True  =
                        trace (unwords [ "PASS"
                                       , show level
                                       , ":"
                                       , pass_name
                                       , show (length (show d))
                                       , "->"
                                       , show (length (show d'))
                                       ])

remove_empty :: Dependency -> Dependency
remove_empty d =
    case d of
        -- drop full empty nodes
        _ | is_empty_dependency d -> empty_dependency
        -- drop partial empty nodes
        DependIfUse use td fd   -> DependIfUse use (go td) (go fd)
        DependAllOf deps        -> DependAllOf $ filter (not . is_empty_dependency) $ map go deps
        DependAnyOf deps        -> DependAnyOf $                                      map go deps
        -- no change
        DependAtom _            -> d
    where go = remove_empty

s_uniq :: [Dependency] -> [Dependency]
s_uniq = S.toList . S.fromList

-- Ideally 'combine_atom_ranges' should handle those as well
remove_duplicates :: Dependency -> Dependency
remove_duplicates d =
    case d of
        DependIfUse use td fd   -> DependIfUse use (go td) (go fd)
        DependAnyOf deps        -> DependAnyOf $ s_uniq $ map go deps
        DependAllOf deps        -> DependAllOf $ s_uniq $ map go deps
        DependAtom  _           -> d
    where go = remove_duplicates

-- TODO: implement flattening AnyOf the same way it's done for AllOf
--   DependAnyOf [DependAnyOf [something], rest] -> DependAnyOf $ something ++ rest
flatten :: Dependency -> Dependency
flatten d =
    case d of
        DependIfUse use td fd   -> DependIfUse use (go td) (go fd)
        DependAnyOf [dep]       -> go dep
        DependAnyOf deps        -> DependAnyOf $ map go deps

        DependAllOf deps        -> case L.partition is_dall_of deps of
                                       ([], [])      -> empty_dependency
                                       ([], [dep])   -> dep
                                       ([], ndall)   -> DependAllOf $ map go ndall
                                       (dall, ndall) -> go $ DependAllOf $ s_uniq $ (concatMap undall dall) ++ ndall
        DependAtom _            -> d
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
combine_atom_ranges :: Dependency -> Dependency
combine_atom_ranges d =
    case d of
        DependIfUse use td fd -> DependIfUse use (go td) (go fd)
        DependAllOf deps      -> DependAllOf $ map go $ find_atom_intersections  deps
        DependAnyOf deps      -> DependAnyOf $ map go $ find_atom_concatenations deps
        DependAtom  _         -> d
    where go = combine_atom_ranges

find_atom_intersections :: [Dependency] -> [Dependency]
find_atom_intersections = map merge_depends . L.groupBy is_mergeable
    where is_mergeable :: Dependency -> Dependency -> Bool
          is_mergeable (DependAtom (Atom lpn _ldrange lattr)) (DependAtom (Atom rpn _rdrange rattr))
                          = (lpn, lattr) == (rpn, rattr)
          is_mergeable _                                       _
                          = False

          merge_depends :: [Dependency] -> Dependency
          merge_depends [x] = x
          merge_depends xs = L.foldl1' merge_pair xs

          merge_pair :: Dependency -> Dependency -> Dependency
          merge_pair (DependAtom (Atom lp ld la)) (DependAtom (Atom rp rd ra))
              | lp /= rp = error "merge_pair got different 'PackageName's"
              | la /= ra = error "merge_pair got different 'DAttr's"
              | otherwise = DependAtom (Atom lp (mergeDRanges ld rd) la)
          merge_pair l r = error $ unwords ["merge_pair can't merge non-atoms:", show l, show r]

-- TODO
find_atom_concatenations :: [Dependency] -> [Dependency]
find_atom_concatenations = id

-- Eliminate use guarded redundancy:
--   a? ( foo )
--   a? ( bar )
-- gets translated to
--   a? ( foo bar )

--   a? ( foo bar )
--   !a? ( foo baz )
-- gets translated to
--   foo
--   a? ( bar )
--   !a? ( baz )

combine_use_guards :: Dependency -> Dependency
combine_use_guards d =
    case d of
        DependIfUse use td fd -> pop_common $ DependIfUse use (go td) (go fd)
        DependAllOf deps      -> DependAllOf $ map go $ find_use_intersections  deps
        DependAnyOf deps      -> DependAnyOf $ map go $ find_use_concatenations deps
        DependAtom _          -> d
    where go = combine_use_guards

find_use_intersections :: [Dependency] -> [Dependency]
find_use_intersections = map merge_use_intersections . L.groupBy is_use_mergeable
    where
        is_use_mergeable :: Dependency -> Dependency -> Bool
        is_use_mergeable (DependIfUse lu _ltd _lfd) (DependIfUse ru _rtd _rfd)
            | lu == ru       = True
        is_use_mergeable _ _ = False

        merge_use_intersections :: [Dependency] -> Dependency
        merge_use_intersections [x] = x
        merge_use_intersections ds = pop_common $ DependIfUse u (DependAllOf tds) (DependAllOf fds)
            where DependIfUse u _tf _fd = head ds
                  tfdeps ~(DependIfUse _u td fd) = (td, fd)
                  (tds, fds) = unzip $ map tfdeps ds

pop_common :: Dependency -> Dependency
-- depend
--   a? ( x ) !a? ( x )
-- gets translated to
--   x
pop_common (DependIfUse _u td fd)
    | td == fd = fd
pop_common d'@(DependIfUse _u td fd) =
    case td_ctx `L.intersect` fd_ctx of
        [] -> d'
        common_ctx -> stabilize_pass flatten $ DependAllOf $ propagate_context' common_ctx d' : common_ctx
    where td_ctx = lift_context' td
          fd_ctx = lift_context' fd
pop_common x = x

-- TODO
find_use_concatenations :: [Dependency] -> [Dependency]
find_use_concatenations = id

-- Eliminate top-down redundancy:
--   foo/bar
--   u? ( foo/bar
--        bar/baz )
-- gets translated to
--   foo/bar
--   u? ( bar/baz )
--
-- and more complex redundancy:
--   v? ( foo/bar )
--   u? ( !v? ( foo/bar ) )
-- gets translated to
--   v? ( foo/bar )
--   u? ( foo/bar )
propagate_context :: Dependency -> Dependency
propagate_context = propagate_context' []

-- very simple model: pick all sibling-atom deps and add them to context
--                    for downward proparation and remove from 'all_of' part
-- TODO: any-of part can benefit from it by removing unsatisfiable or satisfied alternative
propagate_context' :: [Dependency] -> Dependency -> Dependency
propagate_context' ctx d =
    case d of
        _ | d `elem` ctx      -> empty_dependency
        DependIfUse use td fd -> let (t_ctx_comp, t_refined_ctx) = refine_context (True,  use) ctx
                                     (f_ctx_comp, f_refined_ctx) = refine_context (False, use) ctx
                                     tdr = go t_refined_ctx td
                                     fdr = go f_refined_ctx fd
                                     ctx_comp = filter (not . is_empty_dependency) $
                                                concat [ (lift_context' tdr `L.intersect` (concatMap lift_context' t_ctx_comp))
                                                       , (lift_context' fdr `L.intersect` (concatMap lift_context' f_ctx_comp))
                                                       ]
                                     diu_refined = DependIfUse use tdr
                                                                   fdr
                                 in case ctx_comp of
                                    [] -> diu_refined
                                    _  -> go ctx $
                                              DependAllOf [ DependAllOf ctx_comp
                                                          , go ctx_comp diu_refined
                                                          ]
        DependAllOf deps      -> DependAllOf $ fromJust $ msum $
                                                   [ v
                                                   | (optimized_d, other_deps) <- slice_list deps
                                                   , let ctx' = ctx ++ other_deps
                                                         d'   = go ctx' optimized_d
                                                         d'ctx = d' : ctx
                                                         v    = case d' /= optimized_d of
                                                                    True  -> Just (d':map (go d'ctx) other_deps)
                                                                    False -> Nothing -- haven't managed to optimize anything
                                                   ] ++ [Just deps] -- unmodified
        DependAnyOf deps      -> DependAnyOf $ map (go ctx) deps
        DependAtom _          -> case any (dep_as_broad_as d) ctx of
                                     True  -> empty_dependency
                                     False -> d
  where go c = propagate_context' c

-- returns (complement-dependencies, simplified-dependencies)
refine_context :: (Bool, Use) -> [Dependency] -> ([Dependency], [Dependency])
refine_context use_cond = unzip . map (A.second (stabilize_pass flatten) . refine_ctx_unit use_cond)
    where refine_ctx_unit :: (Bool, Use) -> Dependency -> (Dependency, Dependency)
          refine_ctx_unit uc@(bu, u) d =
              case d of
                DependIfUse u' td fd
                  -> case u == u' of
                         False -> ( empty_dependency
                                  , DependIfUse u' (snd $ refine_ctx_unit uc td)
                                                   (snd $ refine_ctx_unit uc fd)
                                  )
                         True  -> case bu of
                                      True  -> (fd, snd $ refine_ctx_unit uc td)
                                      False -> (td, snd $ refine_ctx_unit uc fd)
                _ -> (empty_dependency, d)

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
-- TODO: better add propagation in this exact place to keep tree shrinking only
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
        DependAtom  _            -> d
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
        DependAnyOf deps         -> d : extract_common_constraints (map lift_context' deps)
        DependAtom  _            -> [d]

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
                        | ru < lu && is_empty_dependency rf -> mkUseDependency (True,  ru) $ mkUseDependency (True, lu) (go rt)
                        -- b? ( !a? ( d ) )
                        | ru < lu && is_empty_dependency rt -> mkUseDependency (False, ru) $ mkUseDependency (True, lu) (go rf)
                    _ -> DependIfUse lu (go lt) (go lf)
            | is_empty_dependency lt ->
                case lf of
                    DependIfUse ru rt rf
                        -- !b? ( a? ( d ) )
                        | ru < lu && is_empty_dependency rf -> mkUseDependency (True,  ru) $ mkUseDependency (False, lu) (go rt)
                        -- !b? ( !a? ( d ) )
                        | ru < lu && is_empty_dependency rt -> mkUseDependency (False, ru) $ mkUseDependency (False, lu) (go rf)
                    _ -> DependIfUse lu (go lt) (go lf)
        DependIfUse use td fd   -> DependIfUse use (go td) (go fd)
        DependAnyOf deps        -> DependAnyOf $ L.sort $ map go deps
        DependAllOf deps        -> DependAllOf $ L.sort $ map go deps
        DependAtom  _           -> d
    where go = sort_deps

-- remove various types of redundancy
normalize_depend :: Dependency -> Dependency
normalize_depend = normalize_depend' 50 0 -- arbitrary limit

normalize_depend' :: Int -> Int -> Dependency -> Dependency
normalize_depend' max_level level d
    | level >= max_level = trace "WARNING: Normalize_depend hung up. Optimization is incomplete." d
normalize_depend' max_level level d = next_step next_d
    where next_d = normalization_step level d
          next_step | d == next_d = id
                    | otherwise   = normalize_depend' max_level (level + 1)
