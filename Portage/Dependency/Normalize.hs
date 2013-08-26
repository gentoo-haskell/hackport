module Portage.Dependency.Normalize
  (
    normalize_depend
  ) where

import Data.List ( nub, groupBy )

import Portage.Dependency.Types

mergeDRanges :: DRange -> DRange -> DRange
mergeDRanges _ r@(DExact _) = r
mergeDRanges l@(DExact _) _ = l
mergeDRanges (DRange ll lu) (DRange rl ru) = DRange (max ll rl) (min lu ru)

-- TODO: remove it and switch to 'SatisfiedDepend' instead
empty_dependency :: Dependency
empty_dependency = DependAllOf []

is_empty_dependency :: Dependency -> Bool
is_empty_dependency (DependIfUse _use dep)  =     is_empty_dependency dep
is_empty_dependency (DependAnyOf [])        = True -- because any (const True) == False
is_empty_dependency (DependAnyOf deps)      = any is_empty_dependency deps
is_empty_dependency (DependAllOf deps)      = all is_empty_dependency deps
is_empty_dependency (Atom _pn _dr _dattr)   = False

-- remove one layer of redundancy
normalization_step :: Dependency -> Dependency
normalization_step = combine_atoms . propagate_context . flatten . remove_duplicates . remove_empty

remove_empty :: Dependency -> Dependency
remove_empty d =
    case d of
        -- drop full empty nodes
        _ | is_empty_dependency d -> empty_dependency
        -- drop partial empty nodes
        (DependIfUse use dep)     -> DependIfUse use $                                      remove_empty dep
        (DependAllOf deps)        -> DependAllOf $ filter (not . is_empty_dependency) $ map remove_empty deps
        (DependAnyOf deps)        -> DependAnyOf $                                      map remove_empty deps
        -- no change
        (Atom _pn _dr _dattr)     -> d

-- Ideally 'combine_atoms' should handle those as well
remove_duplicates :: Dependency -> Dependency
remove_duplicates d =
    case d of
        (DependIfUse use dep)     -> (DependIfUse use $ remove_duplicates dep)
        (DependAnyOf deps)        -> DependAnyOf $ nub $ map remove_duplicates deps
        (DependAllOf deps)        -> DependAllOf $ nub $ map remove_duplicates deps
        (Atom _pn _dr _dattr)     -> d

-- TODO: implement flattening (if not done yet in other phases)
--   DependAnyOf [DependAnyOf [something], rest] -> DependAnyOf $ something ++ rest
--   DependAllOf [DependAllOf [something], rest] -> DependAllOf $ something ++ rest
flatten :: Dependency -> Dependency
flatten d =
    case d of
        (DependAnyOf [dep])       -> dep
        (DependAllOf [dep])       -> dep
        -- do nothing
        _                         -> d

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
-- TODO: any-of part can benefit from it by removing useless alternative
-- TODO: analyze different ranges to remove stricter variants like:
--       foo/bar
--       use? ( >=foo/bar-1.0 )
propagate_context' :: [Dependency] -> Dependency -> Dependency
propagate_context' ctx d =
    case d of
        (DependIfUse use dep) -> DependIfUse use (go ctx dep)
        (DependAllOf deps)    -> DependAllOf $ [ go ctx' dep
                                               | dep <- deps
                                               , let atom_deps = [ a
                                                                 | a@(Atom _pn _dp _dattr) <- deps
                                                                 , a /= dep ]
                                               , let ctx' = ctx ++ atom_deps
                                               ]
        (DependAnyOf deps)    -> DependAnyOf $ map (go ctx) deps
        (Atom _pn _dr _dattr) -> case d `elem` ctx of
                                     True  -> empty_dependency
                                     False -> d
  where go c = propagate_context' c

-- remove various types of redundancy
normalize_depend :: Dependency -> Dependency
normalize_depend d = d''
    where d'  = normalization_step d
          d'' | d == d'   =                    d
              | otherwise = normalization_step d'

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
