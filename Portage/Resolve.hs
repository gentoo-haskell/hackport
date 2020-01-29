{-# LANGUAGE PatternGuards #-}

module Portage.Resolve
    ( resolveCategory
    , resolveCategories
    , resolveFullPortageName
    ) where

import qualified Portage.Overlay as Overlay
import qualified Portage.PackageId as Portage

import Distribution.Verbosity
import Distribution.Pretty (prettyShow)
import qualified Distribution.Package as Cabal
import Distribution.Simple.Utils

import qualified Data.Map as Map

import Error

import Debug.Trace (trace)

-- | If a package already exist in the overlay, find which category it has.
-- If it does not exist, we default to \'dev-haskell\'.
resolveCategory :: Verbosity -> Overlay.Overlay -> Cabal.PackageName -> IO Portage.Category
resolveCategory verbosity overlay pn = do
  info verbosity "Searching for which category to use..."
  case resolveCategories overlay pn of
    [] -> do
      info verbosity "No previous version of this package, defaulting category to dev-haskell."
      return devhaskell
    [cat] -> do
      info verbosity $ "Exact match of already existing package, using category: "
                         ++ prettyShow cat
      return cat
    cats -> do
      warn verbosity $ "Multiple matches of categories: " ++ unwords (map prettyShow cats)
      if devhaskell `elem` cats
        then do notice verbosity "Defaulting to dev-haskell"
                return devhaskell
        else do warn verbosity "Multiple matches and no known default. Override by specifying "
                warn verbosity "package category like so  'hackport merge categoryname/package[-version]."
                throwEx (ArgumentError "Specify package category and try again.")
  where
  devhaskell = Portage.Category "dev-haskell"

resolveCategories :: Overlay.Overlay -> Cabal.PackageName -> [Portage.Category]
resolveCategories overlay pn =
  [ cat 
  | (Portage.PackageName cat pn') <- Map.keys om
  , Portage.normalizeCabalPackageName pn == pn'
  ]
  where
    om = Overlay.overlayMap overlay

resolveFullPortageName :: Overlay.Overlay -> Cabal.PackageName -> Maybe Portage.PackageName
resolveFullPortageName overlay pn =
  case resolveCategories overlay pn of
    [] -> Nothing
    [cat] -> ret cat
    cats | (cat:_) <- (filter (`elem` cats) priority) -> ret cat
         | otherwise -> trace ("Ambiguous package name: " ++ show pn ++ ", hits: " ++ show cats) Nothing
  where
  ret c = return (Portage.PackageName c (Portage.normalizeCabalPackageName pn))
  mkC = Portage.Category
  -- if any of these categories show up in the result list, the match isn't
  -- ambiguous, pick the first match in the list
  priority = [ mkC "dev-haskell"
             , mkC "sys-libs"
             , mkC "dev-libs"
             , mkC "x11-libs"
             , mkC "media-libs"
             , mkC "net-libs"
             , mkC "sci-libs"
             ]
