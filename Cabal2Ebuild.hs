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
        ,convertDependency) where

import qualified Distribution.PackageDescription as Cabal
                                                (PackageDescription(..))
import qualified Distribution.Package as Cabal  (PackageIdentifier(..)
                                                , Dependency(..)
                                                , PackageName(..))
import qualified Distribution.Version as Cabal  (VersionRange, foldVersionRange')
import Distribution.Text (display)

import Data.Char          (toLower,isUpper)

import Portage.Dependency
import Portage.Use
import qualified Portage.PackageId as Portage
import qualified Portage.EBuild as Portage
import qualified Portage.Resolve as Portage
import qualified Portage.EBuild as E
import qualified Portage.Overlay as O
import Portage.Version

cabal2ebuild :: Cabal.PackageDescription -> Portage.EBuild
cabal2ebuild pkg = Portage.ebuildTemplate {
    E.name        = map toLower cabalPkgName,
    E.hackage_name= cabalPkgName,
    E.version     = display (Cabal.pkgVersion (Cabal.package pkg)),
    E.description = if null (Cabal.synopsis pkg) then Cabal.description pkg
                                               else Cabal.synopsis pkg,
    E.long_desc       = if null (Cabal.description pkg) then Cabal.synopsis pkg
                                               else Cabal.description pkg,
    E.homepage        = thisHomepage,
    E.license         = Cabal.license pkg,
    E.my_pn           = if any isUpper cabalPkgName then Just cabalPkgName else Nothing,
    E.features        = E.features E.ebuildTemplate
                   ++ (if hasExe then ["bin"] else [])
                   ++ (if hasTests then ["test-suite"] else [])
                   ++ maybe [] (const (["lib","profile","haddock","hoogle"]
                        ++ if cabalPkgName == "hscolour" then [] else ["hscolour"])
                        ) (Cabal.library pkg) -- hscolour can't colour its own sources
  } where
        cabalPkgName = display $ Cabal.pkgName (Cabal.package pkg)
        hasExe = (not . null) (Cabal.executables pkg) 
        hasTests = (not . null) (Cabal.testSuites pkg)
        thisHomepage = if (null $ Cabal.homepage pkg)
                         then E.homepage E.ebuildTemplate
                         else Cabal.homepage pkg

convertDependencies :: O.Overlay -> Portage.Category -> [Cabal.Dependency] -> [Dependency]
convertDependencies overlay category = concatMap (convertDependency overlay category)

convertDependency :: O.Overlay -> Portage.Category -> Cabal.Dependency -> [Dependency]
convertDependency overlay _category (Cabal.Dependency pname@(Cabal.PackageName _name) _)
  | pname `elem` coreLibs = []      -- no explicit dep on core libs
convertDependency overlay category (Cabal.Dependency pname versionRange)
  = convert versionRange
  where
    pn = case Portage.resolveFullPortageName overlay pname of
            Just r  -> r
            Nothing -> Portage.PackageName category (Portage.normalizeCabalPackageName pname)
    convert :: Cabal.VersionRange -> [Dependency]
    convert =  Cabal.foldVersionRange'
             (          [AnyVersionOf                            pn []] -- ^ @\"-any\"@ version
            )(\v     -> [ThisVersionOf      (fromCabalVersion v) pn []] -- ^ @\"== v\"@
            )(\v     -> [LaterVersionOf     (fromCabalVersion v) pn []] -- ^ @\"> v\"@
            )(\v     -> [EarlierVersionOf   (fromCabalVersion v) pn []] -- ^ @\"< v\"@
            )(\v     -> [OrLaterVersionOf   (fromCabalVersion v) pn []] -- ^ @\">= v\"@
            )(\v     -> [OrEarlierVersionOf (fromCabalVersion v) pn []] -- ^ @\"<= v\"@
            )(\v _   -> [ThisMajorOf        (fromCabalVersion v) pn []] -- ^ @\"== v.*\"@ wildcard. (incl lower, excl upper)
            )(\g1 g2 -> [DependEither (flatten g1 ++ flatten g2)   ] -- ^ @\"_ || _\"@ union
            )(\r1 r2 -> r1 ++ r2                                     -- ^ @\"_ && _\"@ intersection
            )(\dp    -> [AllOf dp                                  ] -- ^ @\"(_)\"@ parentheses
            )
      where
      flatten :: [Dependency] -> [[Dependency]]
      flatten [DependEither ds] = concatMap flatten ds
      flatten other = [other]

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
  ,"integer"      -- up to ghc-6.10
  ,"integer-gmp"  -- ghc-6.12+
  ,"old-locale"
  ,"old-time"
  ,"packedstring"
  ,"pretty"
  ,"process"
  -- ,"random"    -- not a core package since ghc-7.2
  ,"rts"
  -- ,"syb"       -- was splitted off from ghc again
  ,"template-haskell"
  ,"unix"         -- unsafe to upgrade
  ]
