module Portage.Dependency (
  Dependency(..),
  showDepend,
  simplify_deps
  ) where

import Portage.Version
import Distribution.Text (display)

import Data.Maybe ( fromJust, catMaybes )
import Data.List ( nub, groupBy, partition, sortBy )
import Data.Ord           (comparing)

type Package = String
type UseFlag = String

data Dependency = AnyVersionOf               Package
                | ThisVersionOf      Version Package   -- =package-version
                | LaterVersionOf     Version Package   -- >package-version
                | EarlierVersionOf   Version Package   -- <package-version
                | OrLaterVersionOf   Version Package   -- >=package-version
                | OrEarlierVersionOf Version Package   -- <=package-version
                | DependEither Dependency Dependency   -- depend || depend
                | DependIfUse  UseFlag    Dependency   -- use? ( depend )
                | ThisMajorOf        Version Package   -- =package-version*
    deriving (Eq,Show)

showDepend :: Dependency -> Package
showDepend (AnyVersionOf         p) = p
showDepend (ThisVersionOf      v p) = "~" ++ p ++ "-" ++ display v
showDepend (LaterVersionOf     v p) = ">" ++ p ++ "-" ++ display v
showDepend (EarlierVersionOf   v p) = "<" ++ p ++ "-" ++ display v
showDepend (OrLaterVersionOf   v p) = ">=" ++ p ++ "-" ++ display v
showDepend (OrEarlierVersionOf v p) = "<=" ++ p ++ "-" ++ display v
showDepend (DependEither       dep1 dep2) = showDepend dep1
                                     ++ " || " ++ showDepend dep2
showDepend (DependIfUse        useflag dep@(DependEither _ _))
                                                = useflag ++ "? " ++ showDepend dep
showDepend (DependIfUse        useflag dep)  = useflag ++ "? ( " ++ showDepend dep++ " )"
showDepend (ThisMajorOf        v p) = "=" ++ p ++ "-" ++ display v ++ "*"

{- Here goes code for dependencies simplification -}

simplify_group_table :: Package ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version -> [Dependency]

-- simplify_group_table p ol       l        e        oe       exact
-- 1) trivial cases:
simplify_group_table    p Nothing  Nothing  Nothing  Nothing  Nothing  = error $ p ++ ": unsolvable constraints"
simplify_group_table    p (Just v) Nothing  Nothing  Nothing  Nothing  = [OrLaterVersionOf v p]
simplify_group_table    p Nothing  (Just v) Nothing  Nothing  Nothing  = [LaterVersionOf v p]
simplify_group_table    p Nothing  Nothing  (Just v) Nothing  Nothing  = [EarlierVersionOf v p]
simplify_group_table    p Nothing  Nothing  Nothing  (Just v) Nothing  = [OrEarlierVersionOf v p]
simplify_group_table    p Nothing  Nothing  Nothing  Nothing  (Just v) = [ThisVersionOf v p]

-- 2) simplification passes
simplify_group_table    p (Just (Version v1 _ _ _)) Nothing (Just (Version v2 _ _ _)) Nothing Nothing
    -- specian case: >=a-v.N a<v.(N+1)   => =a-v.N*
    | (init v1 == init v2) && (last v2 == last v1 + 1) = [ThisMajorOf (Version v1 Nothing [] 0) p]
    | otherwise                                        = [OrLaterVersionOf (Version v1 Nothing [] 0) p, EarlierVersionOf (Version v2 Nothing [] 0) p]

-- TODO: simplify constraints of type: >=a-v1; > a-v2 and such

-- o3) therwise sink:
simplify_group_table    p (Just v)     l@(_)       e@(_)        oe@(_)       exact@(_) =   OrLaterVersionOf v p : simplify_group_table p Nothing  l e oe exact
simplify_group_table    p ol@(Nothing) (Just v)    e@(_)        oe@(_)       exact@(_) =     LaterVersionOf v p : simplify_group_table p ol Nothing e oe exact
simplify_group_table    p ol@(Nothing) l@(Nothing) (Just v)     oe@(_)       exact@(_) =   EarlierVersionOf v p : simplify_group_table p ol l Nothing oe exact
simplify_group_table    p ol@(Nothing) l@(Nothing) e@(Nothing)  (Just v)     exact@(_) = OrEarlierVersionOf v p : simplify_group_table p ol l e Nothing  exact
-- already defined earlier
-- simplify_group_table    p ol@(Nothing) l@(Nothing) e@(Nothing)  oe@(Nothing) (Just v)  = OrEarlierVersionOf v p : simplify_group_table p ol l e oe Nothing

--  >a-v1 >a-v2         => >a-(max v1 v2)
-- key idea: all constraints are enforcing constraints, so we can't get
-- more, than one interval.
simplify_group :: [Dependency] -> [Dependency]
simplify_group [dep@(AnyVersionOf _package)] = [dep]
simplify_group deps = simplify_group_table package
                                           min_or_later_v   -- >=
                                           min_later_v      -- >
                                           max_earlier_v    -- <
                                           max_or_earlier_v -- <=
                                           exact_this_v     -- ==
    where
          package = fromJust.getPackage $ head deps
          max_earlier_v    = safe_minimum $ map earlier_v deps
          max_or_earlier_v = safe_minimum $ map or_earlier_v deps
          min_later_v      = safe_maximum $ map later_v deps
          min_or_later_v   = safe_maximum $ map or_later_v deps
          exact_this_v     = case catMaybes (map this_v deps) of
                                  []  -> Nothing
                                  [v] -> Just v
                                  xs  -> error $ "too many exact versions:" ++ show xs
          --
          earlier_v (EarlierVersionOf v _p) = Just v
          earlier_v _                       = Nothing

          or_earlier_v (OrEarlierVersionOf v _p) = Just v
          or_earlier_v _                         = Nothing

          later_v (LaterVersionOf v _p) = Just v
          later_v _                     = Nothing

          or_later_v (OrLaterVersionOf v _p) = Just v
          or_later_v _                     = Nothing

          this_v (ThisVersionOf v  _p) = Just v
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
simplify_deps deps = (concatMap (simplify_group.nub) $
                       groupBy cmpPkgName $
                         sortBy (comparing getPackageString) groupable)
                     ++ ungroupable
    where (ungroupable, groupable) = partition ((==Nothing).getPackage) deps
          --
          cmpPkgName p1 p2 = cmpMaybe (getPackage p1) (getPackage p2)
          cmpMaybe (Just p1) (Just p2) = p1 == p2
          cmpMaybe _         _         = False
          --
getPackage :: Dependency -> Maybe Package
getPackage (AnyVersionOf package) = Just package
getPackage (ThisVersionOf      _version package) = Just package
getPackage (LaterVersionOf     _version package) = Just package
getPackage (EarlierVersionOf   _version package) = Just package
getPackage (OrLaterVersionOf   _version package) = Just package
getPackage (OrEarlierVersionOf _version package) = Just package
getPackage (DependEither _dependency _Dependency) = Nothing
getPackage (DependIfUse  _useFlag    _Dependency) = Nothing
getPackage (ThisMajorOf        _version package) = Just package
--
getPackageString :: Dependency -> Package
getPackageString dep = maybe "" id $ getPackage dep
