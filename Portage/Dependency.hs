module Portage.Dependency
  (
    simplifyUseDeps
  , sortDeps

  -- reexports
  , module Portage.Dependency.Builder
  , module Portage.Dependency.Print
  , module Portage.Dependency.Types
  ) where

import Data.Function ( on )
import Data.List ( partition, sortBy )
import Data.Maybe ( fromJust, mapMaybe )

import Portage.PackageId

import Portage.Dependency.Builder
import Portage.Dependency.Print
import Portage.Dependency.Types

getPackage :: Dependency -> Maybe PackageName
getPackage (DependAllOf _dependency) = Nothing
getPackage (DependAtom (Atom pn _dr _attrs)) = Just pn
getPackage (DependAnyOf _dependency           ) = Nothing
getPackage (DependIfUse  _useFlag _td _fd) = Nothing

-- | remove all Use dependencies that overlap with normal dependencies
simplifyUseDeps :: [Dependency]         -- list where use deps is taken
                    -> [Dependency]     -- list where common deps is taken
                    -> [Dependency]     -- result deps
simplifyUseDeps ds cs =
    let (u,o) = partition isUseDep ds
        c = mapMaybe getPackage cs
    in (mapMaybe (intersectD c) u)++o

intersectD :: [PackageName] -> Dependency -> Maybe Dependency
intersectD fs (DependIfUse u td fd) =
    case (intersectD fs td, intersectD fs fd) of
        (Nothing,  Nothing)  -> Nothing
        (Just td', Nothing)  -> Just $ DependIfUse u td' empty_dependency
        (Nothing,  Just fd') -> Just $ DependIfUse u empty_dependency fd'
        (Just td', Just fd') -> Just $ DependIfUse u td' fd'
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
isUseDep (DependIfUse _ _ _) = True
isUseDep _ = False


sortDeps :: [Dependency] -> [Dependency]
sortDeps = sortBy dsort . map deeper
  where
    deeper :: Dependency -> Dependency
    deeper (DependIfUse u1 td fd) = DependIfUse u1 (deeper td) (deeper fd)
    deeper (DependAllOf ds)   = DependAllOf $ sortDeps ds
    deeper (DependAnyOf ds)  = DependAnyOf $ sortDeps ds
    deeper x = x
    dsort :: Dependency -> Dependency -> Ordering
    dsort (DependIfUse u1 _ _) (DependIfUse u2 _ _) = u1 `compare` u2
    dsort (DependIfUse _ _ _)  (DependAnyOf _)   = LT
    dsort (DependIfUse _ _ _)  (DependAllOf  _)   = LT
    dsort (DependIfUse _ _ _)  _                  = GT
    dsort (DependAnyOf _)   (DependAnyOf _)   = EQ
    dsort (DependAnyOf _)  (DependIfUse _ _ _)   = GT
    dsort (DependAnyOf _)   (DependAllOf _)    = LT
    dsort (DependAnyOf _)   _                  = GT
    dsort (DependAllOf _)    (DependAllOf _)    = EQ
    dsort (DependAllOf _)    (DependIfUse  _ _ _) = LT
    dsort (DependAllOf _)    (DependAnyOf _)   = GT
    dsort _ (DependIfUse _ _ _)                   = LT
    dsort _ (DependAllOf _)                     = LT
    dsort _ (DependAnyOf _)                    = LT
    dsort a b = (compare `on` getPackage) a b
