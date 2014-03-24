module Portage.Dependency.Normalize
  (
    is_empty_dependency
  , normalize_depend
  ) where

import qualified Data.List as L

import Portage.Dependency.Types
import Portage.Dependency.Builder

mergeDRanges :: DRange -> DRange -> DRange
mergeDRanges _ r@(DExact _) = r
mergeDRanges l@(DExact _) _ = l
mergeDRanges (DRange ll lu) (DRange rl ru) = DRange (max ll rl) (min lu ru)

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

-- TODO: implement flattening (if not done yet in other phases)
--   DependAnyOf [DependAnyOf [something], rest] -> DependAnyOf $ something ++ rest
--   DependAllOf [DependAllOf [something], rest] -> DependAllOf $ something ++ rest
flatten :: Dependency -> Dependency
flatten d =
    case d of
        DependIfUse use td fd   -> DependIfUse use (flatten td) (flatten fd)
        DependAnyOf [dep]       -> flatten dep
        DependAllOf [dep]       -> flatten dep
        DependAnyOf deps        -> DependAnyOf $ map flatten deps
        DependAllOf deps        -> DependAllOf $ map flatten deps
        Atom _pn _dr _dattr     -> d

-- TODO: join atoms with different version boundaries
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
          merge_depends xs = foldl1 merge_pair xs

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
                  common_ctx -> propagate_context $ DependAllOf $ common_ctx ++ [d']
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
-- TODO: remove use-guarded redundancy
--         a? ( x y z )
--         test? ( a? ( y z t ) )
--       can be reduced to
--         a? ( x y z )
--         test? ( a? ( t ) )
propagate_context' :: [Dependency] -> Dependency -> Dependency
propagate_context' ctx d =
    case d of
        DependIfUse use td fd -> DependIfUse use (go ctx td) (go ctx fd)
        DependAllOf deps      -> DependAllOf $ [ go ctx' dep
                                               | dep <- deps
                                               , let atom_deps = [ a
                                                                 | a@(Atom _pn _dp _dattr) <- deps
                                                                 , a /= dep ]
                                               , let ctx' = ctx ++ atom_deps
                                               ]
        DependAnyOf deps      -> DependAnyOf $ map (go ctx) deps
                                 -- 'd' is already satisfied by 'ctx' constraint
        Atom _pn _dr _dattr   -> case any (\ctx_e ->  ctx_e `dep_is_case_of` d) ctx of
                                     True  -> empty_dependency
                                     False -> d
  where go c = propagate_context' c

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

lift_context :: Dependency -> Dependency
lift_context d =
    case d of
        DependIfUse _use _td _fd -> d
        DependAllOf deps         -> DependAllOf $ deps ++ (new_ctx L.\\ deps)
        -- the lift itself
        DependAnyOf _deps        -> case L.null new_ctx of
                                         True  -> d -- nothing is shared downwards
                                         False -> propagate_context $ DependAllOf $ new_ctx ++ [d]
        Atom _pn _dr _dattr      -> d
  where new_ctx = lift_context' d

-- very simple model: pick all sibling-atom deps and add them to context
--                    for upward proparation and intersect with 'all_of' parts
lift_context' :: Dependency -> [Dependency]
lift_context' d =
    case d of
        DependIfUse _use _td _fd -> []
        DependAllOf deps         -> [dep | dep@(Atom _pn _dr _dattr) <- deps]
        DependAnyOf deps         -> case map lift_context' deps of
                                        []    -> []
                                        ctxes -> foldl1 L.intersect ctxes
        Atom _pn _dr _dattr      -> [d]

-- reorders depends to make them more attractive
-- for other normalization algorithms
-- TODO: add all logic from 'sortDeps' here
sort_deps :: Dependency -> Dependency
sort_deps d =
    case d of
        DependIfUse lu lt lf
            | is_empty_dependency lf ->
                case lt of
                    DependIfUse ru rt rf
                        -- b? ( a? ( d ) )
                        | ru < lu && is_empty_dependency rf -> mkUseDependency (True,  ru) $ mkUseDependency (True, lu) rt
                        -- b? ( !a? ( d ) )
                        | ru < lu && is_empty_dependency rt -> mkUseDependency (False, ru) $ mkUseDependency (True, lu) rf
                    _ -> DependIfUse lu (sort_deps lt) (sort_deps lf)
            | is_empty_dependency lt ->
                case lf of
                    DependIfUse ru rt rf
                        -- !b? ( a? ( d ) )
                        | ru < lu && is_empty_dependency rf -> mkUseDependency (True,  ru) $ mkUseDependency (False, lu) rt
                        -- !b? ( !a? ( d ) )
                        | ru < lu && is_empty_dependency rt -> mkUseDependency (False, ru) $ mkUseDependency (False, lu) rf
                    _ -> DependIfUse lu (sort_deps lt) (sort_deps lf)
        DependIfUse use td fd   -> DependIfUse use (sort_deps td) (sort_deps fd)
        DependAnyOf deps        -> DependAnyOf $ map sort_deps deps
        DependAllOf deps        -> DependAllOf $ map sort_deps deps
        Atom _pn _dr _dattr     -> d

-- remove various types of redundancy
normalize_depend :: Dependency -> Dependency
normalize_depend d = next_step next_d
    where next_d = normalization_step d
          next_step | d == next_d = id
                    | otherwise   = normalize_depend
