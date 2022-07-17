{-|
Module      : Cabal2Ebuild
Copyright   : (C) 2005, Duncan Coutts
License     : GPL-2+
Maintainer  : haskell@gentoo.org

A program for generating a Gentoo ebuild from a .cabal file
-}
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.

-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
module Cabal2Ebuild
        (cabal2ebuild
        ,convertDependencies
        ,convertDependency) where

import qualified Distribution.PackageDescription as Cabal
                                                ( PackageDescription(..), license)
import qualified Distribution.Package as Cabal  ( PackageIdentifier(..)
                                                , Dependency(..))
import qualified Distribution.Version as Cabal  ( VersionRange
                                                , cataVersionRange, normaliseVersionRange
                                                , majorUpperBound, mkVersion )
import Distribution.Version (VersionRangeF(..))
import Distribution.Pretty (prettyShow)

import qualified Distribution.Utils.ShortText as ST

import Data.Char          (isUpper)
import Data.Maybe

import Portage.Dependency
import qualified Portage.Cabal as Portage
import qualified Portage.PackageId as Portage
import qualified Portage.EBuild as Portage
import qualified Portage.EBuild.CabalFeature as Portage
import qualified Portage.Resolve as Portage
import qualified Portage.EBuild as E
import qualified Portage.Overlay as O
import Portage.Version

-- | Generate a 'Portage.EBuild' from a 'Portage.Category' and a 'Cabal.PackageDescription'.
cabal2ebuild :: Portage.Category -> Cabal.PackageDescription -> Portage.EBuild
cabal2ebuild cat pkg = Portage.ebuildTemplate {
    E.name        = Portage.cabal_pn_to_PN cabal_pn,
    E.category    = prettyShow cat,
    E.hackage_name= cabalPkgName,
    E.version     = prettyShow (Cabal.pkgVersion (Cabal.package pkg)),
    E.description = ST.fromShortText $ if ST.null (Cabal.synopsis pkg)
                                          then Cabal.description pkg
                                          else Cabal.synopsis pkg,
    E.homepage        = thisHomepage,
    E.license         = Portage.convertLicense $ Cabal.license pkg,
    E.slot            = (E.slot E.ebuildTemplate) ++ (if hasLibs then "/${PV}" else ""),
    E.cabal_pn           = if any isUpper cabalPkgName then Just cabalPkgName else Nothing,
    E.features        = E.features E.ebuildTemplate
                   ++ (if hasLibs then ([ Portage.Lib
                                        , Portage.Profile
                                        , Portage.Haddock
                                        , Portage.Hoogle
                                        ]
                                        ++ if cabalPkgName == "hscolour"
                                                then []
                                                else [Portage.HsColour])
                                  else [])
                   ++ (if hasTests then [Portage.TestSuite]
                                   else [])
  } where
        cabal_pn = Cabal.pkgName $ Cabal.package pkg
        cabalPkgName = prettyShow cabal_pn
        hasLibs = isJust (Cabal.library pkg)
        hasTests = (not . null) (Cabal.testSuites pkg)
        thisHomepage = if (ST.null $ Cabal.homepage pkg)
                         then E.homepage E.ebuildTemplate
                         else ST.fromShortText $ Cabal.homepage pkg

-- | Map 'convertDependency' over ['Cabal.Dependency'].
convertDependencies :: O.Overlay -> Portage.Category -> [Cabal.Dependency] -> [Dependency]
convertDependencies overlay category = map (convertDependency overlay category)

-- | Convert 'Cabal.Dependency' into 'Dependency'.
convertDependency :: O.Overlay -> Portage.Category -> Cabal.Dependency -> Dependency
convertDependency overlay category (Cabal.Dependency pname versionRange _lib)
  = convert versionRange
  where
    pn = case Portage.resolveFullPortageName overlay pname of
            Just r  -> r
            Nothing -> Portage.PackageName category (Portage.normalizeCabalPackageName pname)
    mk_p :: DRange -> Dependency
    mk_p dr = DependAtom (Atom pn dr (DAttr AnySlot []))
    p_v v   = fromCabalVersion v

    convert :: Cabal.VersionRange -> Dependency
    convert = Cabal.cataVersionRange alg . Cabal.normaliseVersionRange
              where
                alg (ThisVersionF v)                = mk_p $ DExact $ p_v v
                alg (LaterVersionF v)               = mk_p $ DRange (StrictLB $ p_v v) InfinityB
                alg (EarlierVersionF v)             = mk_p $ DRange ZeroB $ StrictUB $ p_v v
                alg (OrLaterVersionF v)             = if v == Cabal.mkVersion [0] -- any version
                                                      then mk_p $ DRange ZeroB InfinityB
                                                      else mk_p $ DRange (NonstrictLB $ p_v v)
                                                           InfinityB
                alg (OrEarlierVersionF v)           = mk_p $ DRange ZeroB $ NonstrictUB $ p_v v
                alg (MajorBoundVersionF v)          = mk_p $ DRange (NonstrictLB $ p_v v)
                                                      $ StrictUB $ p_v $ Cabal.majorUpperBound v
                alg (UnionVersionRangesF v1 v2)     = DependAnyOf [v1, v2]
                alg (IntersectVersionRangesF v1 v2) = DependAllOf [v1, v2]
