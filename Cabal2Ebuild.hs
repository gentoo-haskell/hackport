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
        (EBuild(..)
        ,Dependency(..)
        ,cabal2ebuild
        ,convertDependencies
        ,showEBuild) where

import qualified Distribution.PackageDescription as Cabal
                                                (PackageDescription(..))
import qualified Distribution.Package as Cabal  (PackageIdentifier(..)
                                                , Dependency(..)
                                                , PackageName(..))
import qualified Distribution.Version as Cabal  (VersionRange, foldVersionRange', versionBranch, Version)
import qualified Distribution.License as Cabal  (License(..))
import qualified Distribution.Text as Cabal  (display)
--import qualified Distribution.Compiler as Cabal (CompilerFlavor(..))

import Data.Char          (toLower,isUpper)

import Portage.Dependency
import Portage.Version

data EBuild = EBuild {
    name :: String,
    version :: String,
    description :: String,
    homepage :: String,
    src_uri :: String,
    license :: String,
    slot :: String,
    keywords :: [String],
    iuse :: [String],
    depend :: [Dependency],
    rdepend :: [Dependency],
    features :: [String],
    -- comments on various fields for communicating stuff to the user
    licenseComments :: String,
    my_pn :: Maybe String --If the package's name contains upper-case
  }

ebuildTemplate :: EBuild
ebuildTemplate = EBuild {
    name = "foobar",
    version = "0.1",
    description = "",
    homepage = "",
    src_uri = "",
    license = "",
    slot = "0",
    keywords = ["~amd64","~x86"],
    iuse = [],
    depend = [],
    rdepend = [],
    features = [],
    licenseComments = "",
    my_pn = Nothing
  }

cabal2ebuild :: Cabal.PackageDescription -> EBuild
cabal2ebuild pkg = ebuildTemplate {
    name        = map toLower cabalPkgName,
    version     = Cabal.display (Cabal.pkgVersion (Cabal.package pkg)),
    description = if null (Cabal.synopsis pkg) then Cabal.description pkg
                                               else Cabal.synopsis pkg,
    homepage        = Cabal.homepage pkg,
    src_uri         = Cabal.pkgUrl pkg,
    license         = convertLicense (Cabal.license pkg),
    licenseComments = licenseComment (Cabal.license pkg),
    depend          = defaultDepGHC
                    : (simplify_deps $
                         convertDependency (Cabal.Dependency (Cabal.PackageName "Cabal")
                                           (Cabal.descCabalVersion pkg))
                        ++ convertDependencies (Cabal.buildDepends pkg)),
    my_pn           = if any isUpper cabalPkgName then Just cabalPkgName else Nothing,
    features        = features ebuildTemplate
                   ++ (if null (Cabal.executables pkg) then [] else ["bin"])
                   ++ maybe [] (const (["lib","profile","haddock"]
                        ++ if cabalPkgName == "hscolour" then [] else ["hscolour"])
                        ) (Cabal.library pkg) -- hscolour can't colour its own sources
  } where
        cabalPkgName = Cabal.display $ Cabal.pkgName (Cabal.package pkg)

defaultDepGHC :: Dependency
defaultDepGHC     = OrLaterVersionOf (Version [6,6,1] Nothing [] 0) "dev-lang/ghc"

-- map the cabal license type to the gentoo license string format
convertLicense :: Cabal.License -> String
convertLicense (Cabal.GPL mv)     = "GPL-" ++ (maybe "2" show mv)  -- almost certainly version 2
convertLicense (Cabal.LGPL mv)    = "LGPL-" ++ (maybe "2.1" show mv) -- probably version 2.1
convertLicense Cabal.BSD3         = "BSD"
convertLicense Cabal.BSD4         = "BSD-4"
convertLicense Cabal.PublicDomain = "public-domain"
convertLicense Cabal.AllRightsReserved = ""
convertLicense _                  = ""

licenseComment :: Cabal.License -> String
licenseComment Cabal.AllRightsReserved =
  "Note: packages without a license cannot be included in portage"
licenseComment Cabal.OtherLicense =
  "Fixme: \"OtherLicense\", please fill in manually"
licenseComment _ = ""

convertDependencies :: [Cabal.Dependency] -> [Dependency]
convertDependencies = concatMap convertDependency

convertDependency :: Cabal.Dependency -> [Dependency]
convertDependency (Cabal.Dependency pname@(Cabal.PackageName _name) _)
  | pname `elem` coreLibs = []      -- no explicit dep on core libs
convertDependency (Cabal.Dependency pname versionRange)
  = convert versionRange
  where
    -- XXX: not always true, we should look properly for deps in the overlay
    -- to find the correct category
    ebuildName = "dev-haskell/" ++ map toLower (Cabal.display pname)
    convert :: Cabal.VersionRange -> [Dependency]
    convert =  Cabal.foldVersionRange'
             (          [AnyVersionOf                        ebuildName] -- ^ @\"-any\"@ version
            )(\v     -> [ThisVersionOf      (cabalVtoHPv v)  ebuildName] -- ^ @\"== v\"@
            )(\v     -> [LaterVersionOf     (cabalVtoHPv v)  ebuildName] -- ^ @\"> v\"@
            )(\v     -> [EarlierVersionOf   (cabalVtoHPv v)  ebuildName] -- ^ @\"< v\"@
            )(\v     -> [OrLaterVersionOf   (cabalVtoHPv v)  ebuildName] -- ^ @\">= v\"@
            )(\v     -> [OrEarlierVersionOf (cabalVtoHPv v)  ebuildName] -- ^ @\"<= v\"@
{- FIXME -} )(\v1 _  -> [ThisMajorOf        (cabalVtoHPv v1) ebuildName] -- ^ @\"== v.*\"@ wildcard. (incl lower, excl upper)
            )(\r1 r2 -> case (r1,r2) of
                            ([r1'], [r2']) -> [DependEither r1' r2']       -- ^ @\"_ || _\"@ union
                            _              -> error "convertDependency: compound either"
            )(\r1 r2 -> r1 ++ r2
            )

-- converts Cabal version type to hackport version
cabalVtoHPv :: Cabal.Version -> Version
cabalVtoHPv = (\v -> Version v Nothing [] 0) . Cabal.versionBranch

coreLibs :: [Cabal.PackageName]
coreLibs = map Cabal.PackageName
  ["array"
  ,"base"
  ,"bytestring"   -- intentionally no ebuild. use ghc's version
                  -- to avoid dreaded 'diamond dependency' problem
  ,"containers"
  ,"directory"
  --,"editline"
--,"filepath"     --already has ebuild
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

showEBuild :: EBuild -> String
showEBuild ebuild =
  ss "# Copyright 1999-2009 Gentoo Foundation". nl.
  ss "# Distributed under the terms of the GNU General Public License v2". nl.
  ss "# $Header:  $". nl.
  nl.
  ss "CABAL_FEATURES=". quote' (sepBy " " $ features ebuild). nl.
  ss "inherit haskell-cabal". nl.
  nl.
  (case my_pn ebuild of
     Nothing -> id
     Just pn -> ss "MY_PN=". quote pn. nl.
                ss "MY_P=". quote "${MY_PN}-${PV}". nl. nl).
  ss "DESCRIPTION=". quote (description ebuild). nl.
  ss "HOMEPAGE=". quote (homepage ebuild). nl.
  ss "SRC_URI=". quote (replaceVars (src_uri ebuild)).
     (if null (src_uri ebuild) then ss "\t#Fixme: please fill in manually"
         else id). nl.
  nl.
  ss "LICENSE=". quote (license ebuild).
     (if null (licenseComments ebuild) then id
         else ss "\t#". ss (licenseComments ebuild)). nl.
  ss "SLOT=". quote (slot ebuild). nl.
  ss "KEYWORDS=". quote' (sepBy " " $ keywords ebuild).nl.
  ss "IUSE=". quote' (sepBy ", " $ iuse ebuild). nl.
  nl.
  ss "DEPEND=". quote' (sepBy "\n\t\t" $ map showDepend $ depend ebuild). nl.
  (case my_pn ebuild of
     Nothing -> id
     Just _ -> nl. ss "S=". quote ("${WORKDIR}/${MY_P}"). nl)
  $ []
  where replaceVars = replaceCommonVars (name ebuild) (my_pn ebuild) (version ebuild)

ss :: String -> String -> String
ss = showString

sc :: Char -> String -> String
sc = showChar

nl :: String -> String
nl = sc '\n'

quote :: String -> String -> String
quote str = sc '"'. ss str. sc '"'

quote' :: (String -> String) -> String -> String
quote' str = sc '"'. str. sc '"'

sepBy :: String -> [String] -> ShowS
sepBy _ []     = id
sepBy _ [x]    = ss x
sepBy s (x:xs) = ss x. ss s. sepBy s xs

getRestIfPrefix ::
	String ->	-- ^ the prefix
	String ->	-- ^ the string
	Maybe String
getRestIfPrefix (p:ps) (x:xs) = if p==x then getRestIfPrefix ps xs else Nothing
getRestIfPrefix [] rest = Just rest
getRestIfPrefix _ [] = Nothing

subStr ::
	String ->	-- ^ the search string
	String ->	-- ^ the string to be searched
	Maybe (String,String)  -- ^ Just (pre,post) if string is found
subStr sstr str = case getRestIfPrefix sstr str of
	Nothing -> if null str then Nothing else case subStr sstr (tail str) of
		Nothing -> Nothing
		Just (pre,post) -> Just (head str:pre,post)
	Just rest -> Just ([],rest)

replaceMultiVars ::
	[(String,String)] ->	-- ^ pairs of variable name and content
	String ->		-- ^ string to be searched
	String 			-- ^ the result
replaceMultiVars [] str = str
replaceMultiVars whole@((pname,cont):rest) str = case subStr cont str of
	Nothing -> replaceMultiVars rest str
	Just (pre,post) -> (replaceMultiVars rest pre)++pname++(replaceMultiVars whole post)

replaceCommonVars ::
	String ->	-- ^ PN
	Maybe String ->	-- ^ MYPN
	String ->	-- ^ PV
	String ->	-- ^ the string to be replaced
	String
replaceCommonVars pn mypn pv str
	= replaceMultiVars
		([("${P}",pn++"-"++pv)]
		++ maybe [] (\x->[("${MY_P}",x++"-"++pv)]) mypn
		++[("${PN}",pn)]
		++ maybe [] (\x->[("${MY_PN}",x)]) mypn
		++[("${PV}",pv)]) str
