-- A program for generating a Gentoo ebuild from a .cabal file
--
-- Author : Duncan Coutts <dcoutts@gentoo.org>
--
-- Created: 21 July 2005
--
-- Copyright (C) 2005 Duncan Coutts
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- |
-- Maintainer  : haskell@gentoo.org
--
-- cabal2ebuild - a program for generating a Gentoo ebuild from a .cabal file
--
module Cabal2Ebuild
        (cabal2ebuild
        ,convertDependencies
        ,convertDependency
        ,default_ghc_dependency) where

import qualified Distribution.PackageDescription as Cabal
                                                (PackageDescription(..))
import qualified Distribution.Package as Cabal  (PackageIdentifier(..)
                                                , Dependency(..)
                                                , PackageName(..))
import qualified Distribution.Version as Cabal  (VersionRange, foldVersionRange', versionBranch, Version)
import qualified Distribution.License as Cabal  (License(..))
import Distribution.Text (display)

import Data.Char          (toLower,isUpper)

import Portage.Dependency
import qualified Portage.PackageId as Portage
import qualified Portage.EBuild as Portage
import qualified Portage.EBuild as E
import Portage.Version

default_ghc_dependency :: Dependency
default_ghc_dependency = OrLaterVersionOf (Version [6,8,1] Nothing [] 0) (Portage.mkPackageName "dev-lang" "ghc")

cabal2ebuild :: Cabal.PackageDescription -> Portage.EBuild
cabal2ebuild pkg = Portage.ebuildTemplate {
    E.name        = map toLower cabalPkgName,
    E.version     = display (Cabal.pkgVersion (Cabal.package pkg)),
    E.description = if null (Cabal.synopsis pkg) then Cabal.description pkg
                                               else Cabal.synopsis pkg,
    E.homepage        = Cabal.homepage pkg,
    E.src_uri         = Cabal.pkgUrl pkg,
    E.license         = Cabal.license pkg,
    {- E.haskell_deps    = simplify_deps $ convertDependencies (Portage.Category "dev-haskell") (Cabal.buildDepends pkg),
    E.cabal_dep       = head $ convertDependency (Portage.Category "dev-haskell")
                                               (Cabal.Dependency (Cabal.PackageName "Cabal")
                                               (Cabal.descCabalVersion pkg)),
    -}
    E.my_pn           = if any isUpper cabalPkgName then Just cabalPkgName else Nothing,
    E.features        = E.features E.ebuildTemplate
                   ++ (if hasExe then ["bin"] else [])
                   ++ maybe [] (const (["lib","profile","haddock"]
                        ++ if cabalPkgName == "hscolour" then [] else ["hscolour"])
                        ) (Cabal.library pkg) -- hscolour can't colour its own sources
  } where
        cabalPkgName = display $ Cabal.pkgName (Cabal.package pkg)
        -- hasLib = isJust (Cabal.library pkg)
        hasExe = (not . null) (Cabal.executables pkg) 

convertDependencies :: Portage.Category -> [Cabal.Dependency] -> [Dependency]
convertDependencies category = concatMap (convertDependency category)

convertDependency :: Portage.Category -> Cabal.Dependency -> [Dependency]
convertDependency _category (Cabal.Dependency pname@(Cabal.PackageName _name) _)
  | pname `elem` coreLibs = []      -- no explicit dep on core libs
convertDependency category (Cabal.Dependency pname versionRange)
  = convert versionRange
  where
    -- XXX: not always true, we should look properly for deps in the overlay
    -- to find the correct category
    ebuildName = Portage.PackageName category (Portage.normalizeCabalPackageName pname)
    convert :: Cabal.VersionRange -> [Dependency]
    convert =  Cabal.foldVersionRange'
             (          [AnyVersionOf                        ebuildName] -- ^ @\"-any\"@ version
            )(\v     -> [ThisVersionOf      (fromCabalVersion v)  ebuildName] -- ^ @\"== v\"@
            )(\v     -> [LaterVersionOf     (fromCabalVersion v)  ebuildName] -- ^ @\"> v\"@
            )(\v     -> [EarlierVersionOf   (fromCabalVersion v)  ebuildName] -- ^ @\"< v\"@
            )(\v     -> [OrLaterVersionOf   (fromCabalVersion v)  ebuildName] -- ^ @\">= v\"@
            )(\v     -> [OrEarlierVersionOf (fromCabalVersion v)  ebuildName] -- ^ @\"<= v\"@
{- FIXME -} )(\v _   -> [ThisMajorOf        (fromCabalVersion v) ebuildName] -- ^ @\"== v.*\"@ wildcard. (incl lower, excl upper)
            )(\r1 r2 -> case (r1,r2) of
                            ([r1'], [r2']) -> [DependEither r1' r2']       -- ^ @\"_ || _\"@ union
                            _              -> error "convertDependency: compound either"
            )(\r1 r2 -> r1 ++ r2
            )

coreLibs :: [Cabal.PackageName]
coreLibs = map Cabal.PackageName
  ["array"
  ,"base"
  ,"bytestring"   -- intentionally no ebuild. use ghc's version
                  -- to avoid dreaded 'diamond dependency' problem
  ,"containers"
  ,"directory"
  --,"editline"
  ,"filepath"     -- intentionally no ebuild. use ghc's version
  ,"ghc"
  ,"ghc-prim"
  ,"haskell98"
  ,"hpc"          --has ebuild, but only in the overlay
  ,"integer"
  ,"old-locale"
  ,"old-time"
  ,"packedstring"
  ,"pretty"
  ,"process"
  ,"random"
  ,"readline"     --has ebuild, but only in the overlay
  ,"rts"
  ,"syb"          -- intentionally no ebuild. use ghc's version
  ,"template-haskell"
  ,"unix"         --has ebuild, but only in the overlay
  ]
